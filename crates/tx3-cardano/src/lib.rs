use std::collections::HashMap;

pub mod coercion;
pub mod compile;

// Re-export pallas for upstream users
pub use pallas;

use pallas::ledger::primitives;
use pallas::ledger::traverse::ComputeHash;
use tx3_lang::backend::TxEval;
use tx3_lang::ir;

#[cfg(test)]
pub mod tests;

pub type Network = pallas::ledger::primitives::NetworkId;
pub type PlutusVersion = u8;
pub type CostModel = Vec<i64>;

pub struct PParams {
    pub network: Network,
    pub min_fee_coefficient: u64,
    pub min_fee_constant: u64,
    pub coins_per_utxo_byte: u64,
    pub cost_models: HashMap<PlutusVersion, CostModel>,
}

pub const EXECUTION_UNITS: primitives::ExUnits = primitives::ExUnits {
    mem: 2000000,
    steps: 2000000000,
};

const DEFAULT_EXTRA_FEES: u64 = 200_000;
const MIN_UTXO_BYTES: i128 = 197;

#[derive(Debug, Clone, Default)]
pub struct Config {
    pub extra_fees: Option<u64>,
}

pub type TxBody =
    pallas::codec::utils::KeepRaw<'static, primitives::conway::TransactionBody<'static>>;

pub struct Compiler {
    pub pparams: PParams,
    pub config: Config,
    pub latest_tx_body: Option<TxBody>,
}

impl Compiler {
    pub fn new(pparams: PParams, config: Config) -> Self {
        Self {
            pparams,
            config,
            latest_tx_body: None,
        }
    }
}

impl tx3_lang::backend::Compiler for Compiler {
    fn compile(&mut self, tx: &tx3_lang::ir::Tx) -> Result<TxEval, tx3_lang::backend::Error> {
        let tx = compile::entry_point(tx, &self.pparams)?;

        self.latest_tx_body = Some(tx.transaction_body.clone());

        let hash = tx.transaction_body.compute_hash();

        let payload = pallas::codec::minicbor::to_vec(&tx).unwrap();

        let size_fees = eval_size_fees(&payload, &self.pparams, self.config.extra_fees);

        //let redeemer_fees = eval_redeemer_fees(tx, pparams)?;

        let eval = TxEval {
            payload,
            hash: hash.to_vec(),
            fee: size_fees, // TODO: add redeemer fees
            ex_units: 0,
        };

        Ok(eval)
    }

    fn execute(&self, op: ir::CompilerOp) -> Result<ir::Expression, tx3_lang::backend::Error> {
        match op {
            ir::CompilerOp::BuildScriptAddress(x) => {
                let hash: primitives::Hash<28> = coercion::expr_into_hash(&x)?;
                let address = coercion::policy_into_address(hash.as_ref(), self.pparams.network)?;
                Ok(ir::Expression::Address(address.to_vec()))
            }
            ir::CompilerOp::ComputeMinUtxo(x) => {
                let lovelace = compile::compute_min_utxo(
                    x,
                    &self.latest_tx_body,
                    self.pparams.coins_per_utxo_byte as i128,
                )?;
                Ok(ir::Expression::Assets(vec![ir::AssetExpr {
                    policy: ir::Expression::None,
                    asset_name: ir::Expression::None,
                    amount: ir::Expression::Number(lovelace),
                }]))
            }
        }
    }
}

fn eval_size_fees(tx: &[u8], pparams: &PParams, extra_fees: Option<u64>) -> u64 {
    tx.len() as u64 * pparams.min_fee_coefficient
        + pparams.min_fee_constant
        + extra_fees.unwrap_or(DEFAULT_EXTRA_FEES)
}
