use std::collections::HashMap;

use tx3_lang::{ir::InputQuery, UtxoSet};

use crate::{resolve::Ledger, Error, PParams};

const COST_MODEL_PLUTUS_V1: [i64; 166] = [
    100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4, 16000, 100,
    16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100, 16000, 100, 94375, 32,
    132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848, 228465, 122, 0, 1,
    1, 1000, 42921, 4, 2, 24548, 29498, 38, 1, 898148, 27279, 1, 51775, 558, 1, 39184, 1000, 60594,
    1, 141895, 32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10, 28999, 74, 1, 28999, 74, 1,
    43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391, 32, 11546, 32,
    85848, 228465, 122, 0, 1, 1, 90434, 519, 0, 1, 74433, 32, 85848, 228465, 122, 0, 1, 1, 85848,
    228465, 122, 0, 1, 1, 270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788,
    420, 1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32, 25933, 32, 24623, 32,
    53384111, 14333, 10,
];

static COST_MODEL_PLUTUS_V2: [i64; 175] = [
    100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4, 16000, 100,
    16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100, 16000, 100, 94375, 32,
    132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848, 228465, 122, 0, 1,
    1, 1000, 42921, 4, 2, 24548, 29498, 38, 1, 898148, 27279, 1, 51775, 558, 1, 39184, 1000, 60594,
    1, 141895, 32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10, 28999, 74, 1, 28999, 74, 1,
    43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391, 32, 11546, 32,
    85848, 228465, 122, 0, 1, 1, 90434, 519, 0, 1, 74433, 32, 85848, 228465, 122, 0, 1, 1, 85848,
    228465, 122, 0, 1, 1, 955506, 213312, 0, 2, 270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4,
    0, 141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32, 25933,
    32, 24623, 32, 43053543, 10, 53384111, 14333, 10, 43574283, 26308, 10,
];

#[derive(Debug, Default)]
pub struct MockLedger {
    pub default_datum: Option<tx3_lang::ir::Expression>,
}

impl Ledger for MockLedger {
    async fn get_pparams(&self) -> Result<PParams, Error> {
        Ok(PParams {
            network: crate::Network::Testnet,
            min_fee_coefficient: 1,
            min_fee_constant: 2,
            coins_per_utxo_byte: 1,
            cost_models: HashMap::from([
                (0, COST_MODEL_PLUTUS_V1.to_vec()),
                (1, COST_MODEL_PLUTUS_V2.to_vec()),
                // using the same cost model for v3 for now, need to lookup a valid one
                (2, COST_MODEL_PLUTUS_V2.to_vec()),
            ]),
        })
    }

    async fn resolve_input(&self, _input: &InputQuery) -> Result<UtxoSet, Error> {
        let utxo = tx3_lang::Utxo {
        r#ref: tx3_lang::UtxoRef {
            txid: hex::decode(
                    "267aae354f0d14d82877fa5720f7ddc9b0e3eea3cd2a0757af77db4d975ba81c",
                )
                .unwrap().clone(),
            index: 0,
        },
        address: pallas::ledger::addresses::Address::from_bech32("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2").unwrap().to_vec(),
        datum: self.default_datum.clone(),
        assets: vec![tx3_lang::ir::AssetExpr {
            policy: tx3_lang::ir::Expression::None,
            asset_name: tx3_lang::ir::Expression::None,
            amount: tx3_lang::ir::Expression::Number(500_000_000_i128),
        }],
        script: None
    };

        Ok(UtxoSet::from([utxo]))
    }
}
