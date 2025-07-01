use serde::{Deserialize, Serialize};
use std::{collections::HashMap, sync::Arc};
use tx3_lang::ir::InputQuery;

use tokio::sync::Mutex;
use utxorpc::CardanoQueryClient;

use crate::PParams;

impl From<utxorpc::Error> for crate::Error {
    fn from(error: utxorpc::Error) -> Self {
        crate::Error::LedgerInternalError(error.to_string())
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Config {
    pub endpoint_url: String,
    pub api_key: String,
    pub network_id: u8,
}

fn expr_to_address_pattern(
    value: &tx3_lang::ir::Expression,
) -> Option<utxorpc::spec::cardano::AddressPattern> {
    let address = match value {
        tx3_lang::ir::Expression::Address(address) => address.clone(),
        tx3_lang::ir::Expression::String(address) => {
            pallas::ledger::addresses::Address::from_bech32(address)
                .unwrap()
                .to_vec()
        }
        _ => return None,
    };

    let out = utxorpc::spec::cardano::AddressPattern {
        exact_address: address.into(),
        ..Default::default()
    };

    Some(out)
}

fn input_query_to_pattern(query: &InputQuery) -> utxorpc::spec::cardano::TxOutputPattern {
    let address = expr_to_address_pattern(&query.address);

    utxorpc::spec::cardano::TxOutputPattern {
        address,
        ..Default::default()
    }
}

fn utxo_from_u5c_to_tx3(u: utxorpc::ChainUtxo<utxorpc::spec::cardano::TxOutput>) -> tx3_lang::Utxo {
    tx3_lang::Utxo {
        r#ref: tx3_lang::UtxoRef {
            txid: u.txo_ref.as_ref().unwrap().hash.clone().into(),
            index: u.txo_ref.as_ref().unwrap().index,
        },
        address: u.parsed.as_ref().unwrap().address.clone().into(),
        datum: None, //u.parsed.unwrap().datum.into(),
        assets: vec![tx3_lang::ir::AssetExpr {
            policy: tx3_lang::ir::Expression::None,
            asset_name: tx3_lang::ir::Expression::None,
            amount: tx3_lang::ir::Expression::Number(u.parsed.as_ref().unwrap().coin as i128),
        }], //u.parsed.unwrap().assets.into(),
        script: u
            .parsed
            .as_ref()
            .and_then(|x| x.script.as_ref())
            .and_then(|x| x.script.as_ref())
            .map(|x| {
                let mut buf = vec![];
                x.encode(&mut buf);
                buf
            })
            .map(tx3_lang::ir::Expression::Bytes),
    }
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

        let out = match params {
            utxorpc::spec::query::any_chain_params::Params::Cardano(params) => PParams {
                network: crate::Network::try_from(self.network_id).unwrap(),
                min_fee_coefficient: params.min_fee_coefficient,
                min_fee_constant: params.min_fee_constant,
                coins_per_utxo_byte: params.coins_per_utxo_byte,
                cost_models: HashMap::from([
                    (
                        1,
                        params
                            .cost_models
                            .as_ref()
                            .and_then(|cm| cm.plutus_v1.as_ref())
                            .map(|cm| cm.values.clone())
                            .unwrap_or_default(),
                    ),
                    (
                        2,
                        params
                            .cost_models
                            .as_ref()
                            .and_then(|cm| cm.plutus_v2.as_ref())
                            .map(|cm| cm.values.clone())
                            .unwrap_or_default(),
                    ),
                    (
                        3,
                        params
                            .cost_models
                            .as_ref()
                            .and_then(|cm| cm.plutus_v3.as_ref())
                            .map(|cm| cm.values.clone())
                            .unwrap_or_default(),
                    ),
                ]),
            },
        };

        Ok(out)
    }

    async fn resolve_input(&self, query: &InputQuery) -> Result<tx3_lang::UtxoSet, crate::Error> {
        let utxos = self
            .queries
            .lock()
            .await
            .match_utxos(input_query_to_pattern(query), None, 1)
            .await?
            .items
            .into_iter()
            .map(utxo_from_u5c_to_tx3)
            .collect();

        Ok(utxos)
    }
}
