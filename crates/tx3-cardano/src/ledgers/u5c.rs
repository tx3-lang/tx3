use serde::{Deserialize, Serialize};
use std::sync::Arc;

use tokio::sync::Mutex;
use utxorpc::CardanoQueryClient;

use crate::PParams;

impl From<utxorpc::Error> for crate::Error {
    fn from(error: utxorpc::Error) -> Self {
        crate::Error::LedgerInternalError(error.to_string())
    }
}

fn expr_to_address_pattern(
    value: &tx3_lang::ir::Expression,
) -> utxorpc::spec::cardano::AddressPattern {
    match value {
        tx3_lang::ir::Expression::Address(address) => utxorpc::spec::cardano::AddressPattern {
            exact_address: address.clone().into(),
            ..Default::default()
        },
        tx3_lang::ir::Expression::String(address) => utxorpc::spec::cardano::AddressPattern {
            exact_address: pallas::ledger::addresses::Address::from_bech32(address)
                .unwrap()
                .to_vec()
                .into(),
            ..Default::default()
        },
        tx3_lang::ir::Expression::Bytes(address) => utxorpc::spec::cardano::AddressPattern {
            exact_address: address.clone().into(),
            ..Default::default()
        },
        _ => Default::default(),
    }
}

fn utxo_from_u5c_to_tx3(u: utxorpc::ChainUtxo<utxorpc::spec::cardano::TxOutput>) -> tx3_lang::Utxo {
    tx3_lang::Utxo {
        r#ref: tx3_lang::UtxoRef {
            txid: u.txo_ref.as_ref().unwrap().hash.clone().into(),
            index: u.txo_ref.as_ref().unwrap().index as u32,
        },
        address: u.parsed.as_ref().unwrap().address.clone().into(),
        datum: None, //u.parsed.unwrap().datum.into(),
        assets: vec![tx3_lang::ir::AssetExpr {
            policy: vec![],
            asset_name: tx3_lang::ir::Expression::Bytes(vec![]),
            amount: tx3_lang::ir::Expression::Number(u.parsed.as_ref().unwrap().coin as i128),
        }], //u.parsed.unwrap().assets.into(),
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Config {
    pub endpoint_url: String,
    pub api_key: String,
    pub network_id: u8,
}

#[derive(Clone)]
pub struct Ledger {
    queries: Arc<Mutex<utxorpc::CardanoQueryClient>>,
    network_id: u8,
}

impl Ledger {
    pub async fn new(config: Config) -> Result<Self, crate::Error> {
        let queries = utxorpc::ClientBuilder::new()
            .uri(&config.endpoint_url)?
            .metadata("dmtr-api-key", config.api_key)?
            .build::<CardanoQueryClient>()
            .await;

        Ok(Self {
            queries: Arc::new(Mutex::new(queries)),
            network_id: config.network_id,
        })
    }

    // pub async fn read_utxos(
    //     &mut self,
    //     refs: Vec<wit::TxoRef>,
    // ) -> Result<Vec<wit::Utxo>, wit::LedgerError> {
    //     let refs = refs.into_iter().map(|r| r.into()).collect();
    //     let utxos = self.queries.read_utxos(refs).await?;
    //     Ok(utxos.into_iter().map(|u| u.into()).collect())
    // }
}

impl crate::resolve::Ledger for Ledger {
    async fn get_pparams(&self) -> Result<PParams, crate::Error> {
        let req = utxorpc::spec::query::ReadParamsRequest::default();

        let res = self
            .queries
            .lock()
            .await
            .read_params(req)
            .await
            .map_err(|err| crate::Error::LedgerInternalError(format!("{:?}", err)))?;

        let params = res.into_inner().values.and_then(|v| v.params).ok_or(
            crate::Error::LedgerInternalError("unexpected response from read_params".to_string()),
        )?;

        match params {
            utxorpc::spec::query::any_chain_params::Params::Cardano(params) => Ok(PParams {
                network: pallas::ledger::addresses::Network::from(self.network_id),
                min_fee_coefficient: params.min_fee_coefficient,
                min_fee_constant: params.min_fee_constant,
                coins_per_utxo_byte: params.coins_per_utxo_byte,
            }),
        }
    }

    async fn resolve_input(
        &self,
        query: &tx3_lang::ir::InputQuery,
    ) -> Result<tx3_lang::UtxoSet, crate::Error> {
        let address = query.address.as_ref().map(|x| expr_to_address_pattern(x));

        let pattern = utxorpc::spec::cardano::TxOutputPattern {
            address: address,
            ..Default::default()
        };

        dbg!(&pattern);

        let utxos = self
            .queries
            .lock()
            .await
            .match_utxos(pattern, None, 1)
            .await?;

        let utxos = utxos.items.into_iter().map(utxo_from_u5c_to_tx3).take(1);

        // TODO: actually filter utxos
        Ok(utxos.collect())
    }
}
