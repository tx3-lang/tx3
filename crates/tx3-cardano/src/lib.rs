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

#[derive(Debug, Clone)]
pub struct ChainPoint {
    pub slot: u64,
    pub hash: Vec<u8>,
    pub timestamp: u128,
}

pub struct Compiler {
    pub pparams: PParams,
    pub config: Config,
    pub latest_tx_body: Option<TxBody>,
    pub cursor: ChainPoint,
}

impl Compiler {
    pub fn new(pparams: PParams, config: Config, cursor: ChainPoint) -> Self {
        Self {
            pparams,
            config,
            latest_tx_body: None,
            cursor,
        }
    }
}

impl tx3_lang::backend::Compiler for Compiler {
    fn compile(&mut self, tx: &tx3_lang::ir::Tx) -> Result<TxEval, tx3_lang::backend::Error> {
        let compiled_tx = compile::entry_point(tx, &self.pparams)?;

        let hash = compiled_tx.transaction_body.compute_hash();
        let payload = pallas::codec::minicbor::to_vec(&compiled_tx).unwrap();

        self.latest_tx_body = Some(compiled_tx.transaction_body);

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
            ir::CompilerOp::ComputeTipSlot => Ok(ir::Expression::Number(self.cursor.slot as i128)),
            ir::CompilerOp::ComputeSlotsToUnixTime(x) => {
                let slot_i128 = coercion::expr_into_number(&x)?;
                if slot_i128 < 0 {
                    return Err(tx3_lang::backend::Error::CoerceError(
                        format!("{}", slot_i128),
                        "positive slot number".to_string(),
                    ));
                }

                let current_time = self.cursor.timestamp as i128;
                let time_diff = slot_i128 - self.cursor.slot as i128;
                let unix_time_millis = current_time + (time_diff * 1000);

                Ok(ir::Expression::Number(unix_time_millis))
            }
            ir::CompilerOp::ComputeUnixTimeToSlots(x) => {
                let unix_time_i128 = coercion::expr_into_number(&x)?;
                if unix_time_i128 < 0 {
                    return Err(tx3_lang::backend::Error::CoerceError(
                        format!("{}", unix_time_i128),
                        "positive unix timestamp".to_string(),
                    ));
                }
                let unix_time = unix_time_i128 as u64;
                let slot =
                    compute_unix_time_to_slot(unix_time, self.cursor.slot, self.cursor.timestamp)?;
                Ok(ir::Expression::Number(slot as i128))
            }
        }
    }
}

fn eval_size_fees(tx: &[u8], pparams: &PParams, extra_fees: Option<u64>) -> u64 {
    tx.len() as u64 * pparams.min_fee_coefficient
        + pparams.min_fee_constant
        + extra_fees.unwrap_or(DEFAULT_EXTRA_FEES)
}

fn compute_unix_time_to_slot(
    unix_time: u64,
    current_tip_slot: u64,
    tip_timestamp: u128,
) -> Result<u64, tx3_lang::backend::Error> {
    let current_time = tip_timestamp as i128;

    let time_diff = (unix_time as i128 - current_time) / 1000;
    let slot = (current_tip_slot as i128 + time_diff) as u64;

    Ok(slot)
}
