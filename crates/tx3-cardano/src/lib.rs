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

struct SlotConfig {
    zero_time: i64,
    zero_slot: i64,
    slot_length: i64,
}

const MAINNET_SLOT_CONFIG: SlotConfig = SlotConfig {
    zero_time: 1596059091000,
    zero_slot: 4492800,
    slot_length: 1000,
};

const TESTNET_SLOT_CONFIG: SlotConfig = SlotConfig {
    zero_time: 1666656000000,
    zero_slot: 0,
    slot_length: 1000,
};

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
            ir::CompilerOp::ComputeTipSlot => {
                let slot = compute_tip_slot(self.pparams.network)?;
                Ok(ir::Expression::Number(slot as i128))
            }
        }
    }
}

fn compute_tip_slot(network: Network) -> Result<i64, tx3_lang::backend::Error> {
    use std::time::{SystemTime, UNIX_EPOCH};

    let current_time_ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|_| tx3_lang::backend::Error::CantReduce(ir::CompilerOp::ComputeTipSlot))?
        .as_millis() as i64;

    let config = match network {
        Network::Mainnet => &MAINNET_SLOT_CONFIG,
        Network::Testnet => &TESTNET_SLOT_CONFIG,
    };

    let elapsed_time = current_time_ms - config.zero_time;
    let slot = config.zero_slot + (elapsed_time / config.slot_length);

    Ok(slot.max(0))
}

fn eval_size_fees(tx: &[u8], pparams: &PParams, extra_fees: Option<u64>) -> u64 {
    tx.len() as u64 * pparams.min_fee_coefficient
        + pparams.min_fee_constant
        + extra_fees.unwrap_or(DEFAULT_EXTRA_FEES)
}
