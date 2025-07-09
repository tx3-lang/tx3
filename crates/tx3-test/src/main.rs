//! Driver to serve JSON-RPC requests.
//!
//! This driver implements an HTTP server that listens for JSON-RPC requests
//! and funnels them into the Runtime. The path of the request is used as the
//! key to identify the worker that should handle the request. The JSON-RPC
//! method field is used as the key to identify the particular Balius request
//! for the worker. JSON-RPC params are mapped directly into Balius request
//! params.
//!
//! The JSON-RPC server is implemented as a Warp application and adheres to
//! the JSON-RPC 2.0 spec.

use base64::Engine;
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::net::SocketAddr;
use tokio_util::sync::CancellationToken;
use tracing::{debug, error};
use warp::Filter as _;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("config error: {0}")]
    Config(String),

    #[error("invalid request for method: {0}")]
    InvalidRequestForMethod(String),

    #[error("ledger error: {0}")]
    LedgerError(String),

    #[error("invalid ir: {0}")]
    InvalidIr(String),

    #[error("resolve error: {0}")]
    ResolveError(#[from] tx3_cardano::Error),

    #[error("unknown method: {0}")]
    UnknownMethod(String),

    #[error("invalid arg value {0}: {1}")]
    InvalidArg(String, String),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Config {
    pub listen_address: String,
    pub ledger: tx3_cardano::ledgers::u5c::Config,
}

#[derive(Deserialize)]
struct AnyRequest {
    pub id: Option<String>,
    pub method: String,
    pub params: serde_json::Value,
}

#[derive(Deserialize)]
enum IrEncoding {
    #[serde(rename = "base64")]
    Base64,
    #[serde(rename = "hex")]
    Hex,
}

#[derive(Deserialize)]
struct IrEnvelope {
    #[allow(dead_code)]
    pub version: String,

    pub bytecode: String,
    pub encoding: IrEncoding,
}

#[derive(Deserialize)]
struct ResolveProtoTxRequest {
    pub tir: IrEnvelope,
    pub args: serde_json::Value,
}

impl TryFrom<AnyRequest> for ResolveProtoTxRequest {
    type Error = Error;

    fn try_from(request: AnyRequest) -> Result<Self, Self::Error> {
        serde_json::from_value(request.params)
            .map_err(|x| Error::InvalidRequestForMethod(x.to_string()))
    }
}

#[derive(Serialize)]
struct ErrorResponse {
    error: String,
}

impl From<Error> for ErrorResponse {
    fn from(err: Error) -> Self {
        ErrorResponse {
            error: err.to_string(),
        }
    }
}

fn parse_request(body: serde_json::Value) -> Result<AnyRequest, ErrorResponse> {
    match serde_json::from_value(body) {
        Ok(x) => Ok(x),
        Err(x) => Err(ErrorResponse {
            error: x.to_string(),
        }),
    }
}

async fn handle_resolve_proto_tx(
    ledger: tx3_cardano::ledgers::u5c::Ledger,
    request: AnyRequest,
) -> Result<warp::reply::Json, Error> {
    let request = ResolveProtoTxRequest::try_from(request)?;

    let tx = match request.tir.encoding {
        IrEncoding::Base64 => base64::engine::general_purpose::STANDARD
            .decode(request.tir.bytecode)
            .map_err(|x| Error::InvalidIr(x.to_string()))?,
        IrEncoding::Hex => {
            hex::decode(request.tir.bytecode).map_err(|x| Error::InvalidIr(x.to_string()))?
        }
    };

    let mut tx =
        tx3_lang::ProtoTx::from_ir_bytes(&tx).map_err(|x| Error::InvalidIr(x.to_string()))?;

    for (key, val) in request.args.as_object().unwrap().iter() {
        match val {
            serde_json::Value::String(x) => tx.set_arg(key, x.as_str().into()),
            serde_json::Value::Number(x) => tx.set_arg(key, x.as_i64().unwrap().into()),
            _ => return Err(Error::InvalidArg(key.to_string(), val.to_string())),
        }
    }

    let tx = tx3_cardano::resolve_tx(
        tx,
        ledger,
        tx3_cardano::resolve::Config {
            max_optimize_rounds: 5,
            extra_fees: None,
        },
    )
    .await?;

    let reply = json!({ "tx": hex::encode(tx.payload) });

    Ok(warp::reply::json(&reply))
}

pub async fn handle_request(
    ledger: tx3_cardano::ledgers::u5c::Ledger,
    body: serde_json::Value,
) -> warp::reply::Json {
    let request = match parse_request(body) {
        Ok(x) => x,
        Err(err) => return warp::reply::json(&err),
    };

    debug!(id = request.id, method = request.method, "handling request");

    let result = match request.method.as_str() {
        "trp.resolve" => handle_resolve_proto_tx(ledger, request).await,
        x => Err(Error::UnknownMethod(x.to_string())),
    };

    match result {
        Ok(x) => x,
        Err(err) => warp::reply::json(&ErrorResponse::from(err)),
    }
}

pub async fn serve(config: Config, cancel: CancellationToken) -> Result<(), Error> {
    let ledger = tx3_cardano::ledgers::u5c::Ledger::new(config.ledger)
        .await
        .unwrap();

    let cors = warp::cors()
        .allow_any_origin()
        .allow_headers(vec!["content-type"])
        .allow_methods(vec!["POST", "GET", "OPTIONS"]);

    let filter = warp::any()
        .map(move || ledger.clone())
        .and(warp::post())
        .and(warp::body::json())
        .then(handle_request)
        .with(cors);

    let address: SocketAddr = config
        .listen_address
        .parse()
        .map_err(|x: std::net::AddrParseError| Error::Config(x.to_string()))?;

    let (addr, server) =
        warp::serve(filter).bind_with_graceful_shutdown(address, cancel.cancelled_owned());

    tracing::info!(%addr, "Json-RPC server listening");

    server.await;

    Ok(())
}

#[tokio::main]
async fn main() {
    let config = Config {
        listen_address: "0.0.0.0:8000".to_string(),
        ledger: tx3_cardano::ledgers::u5c::Config {
            endpoint_url: "https://mainnet.utxorpc-v0.demeter.run".to_string(),
            api_key: "dmtr_utxorpc1wgnnj0qcfj32zxsz2uc8d4g7uclm2s2w".to_string(),
            network_id: 1,
        },
    };

    serve(config, CancellationToken::new()).await.unwrap();
}
