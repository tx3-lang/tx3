use std::collections::HashMap;

pub mod coercion;
pub mod compile;
pub mod ops;

// Re-export pallas for upstream users
pub use pallas;

use pallas::ledger::primitives;
use pallas::ledger::traverse::ComputeHash;
use tx3_tir::compile::CompiledTx;
use tx3_tir::compile::Error as CompileError;
use tx3_tir::encoding::AnyTir;
use tx3_tir::model::v1beta0 as tir;
use tx3_tir::reduce::Error as ReduceError;

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

impl tx3_tir::compile::Compiler for Compiler {
    type CompilerOp = tir::CompilerOp;
    type Expression = tir::Expression;

    fn compile(&mut self, tir: &AnyTir) -> Result<CompiledTx, CompileError> {
        let AnyTir::V1Beta0(tx) = tir else {
            return Err(CompileError::UnsupportedTirVersion(tir.version()));
        };

        let compiled_tx = compile::entry_point(tx, &self.pparams)?;

        let hash = compiled_tx.transaction_body.compute_hash();
        let payload = pallas::codec::minicbor::to_vec(&compiled_tx).unwrap();

        self.latest_tx_body = Some(compiled_tx.transaction_body);

        let size_fees = ops::eval_size_fees(&payload, &self.pparams, self.config.extra_fees);

        //let redeemer_fees = eval_redeemer_fees(tx, pparams)?;

        let eval = CompiledTx {
            payload,
            hash: hash.to_vec(),
            fee: size_fees, // TODO: add redeemer fees
            ex_units: 0,
        };

        Ok(eval)
    }

    fn reduce_op(&self, op: Self::CompilerOp) -> Result<Self::Expression, ReduceError> {
        match op {
            tir::CompilerOp::BuildScriptAddress(x) => {
                let hash: primitives::Hash<28> = coercion::expr_into_hash(&x)?;
                let address = coercion::policy_into_address(hash.as_ref(), self.pparams.network)?;
                Ok(tir::Expression::Address(address.to_vec()))
            }
            tir::CompilerOp::ComputeMinUtxo(x) => {
                let lovelace = ops::compute_min_utxo(
                    x,
                    &self.latest_tx_body,
                    self.pparams.coins_per_utxo_byte as i128,
                )?;

                Ok(tir::Expression::Assets(vec![tir::AssetExpr {
                    policy: tir::Expression::None,
                    asset_name: tir::Expression::None,
                    amount: tir::Expression::Number(lovelace),
                }]))
            }
            tir::CompilerOp::ComputeTipSlot => {
                Ok(tir::Expression::Number(self.cursor.slot as i128))
            }
            tir::CompilerOp::ComputeSlotToTime(x) => {
                let slot = coercion::expr_into_number(&x)?;

                if slot < 0 {
                    return Err(CompileError::CoerceError(
                        format!("{}", slot),
                        "positive slot number".to_string(),
                    )
                    .into());
                }

                Ok(tir::Expression::Number(ops::slot_to_time(
                    slot,
                    &self.cursor,
                )))
            }
            tir::CompilerOp::ComputeTimeToSlot(x) => {
                let time = coercion::expr_into_number(&x)?;
                if time < 0 {
                    return Err(CompileError::CoerceError(
                        format!("{}", time),
                        "positive timestamp".to_string(),
                    )
                    .into());
                }

                Ok(tir::Expression::Number(ops::time_to_slot(
                    time,
                    &self.cursor,
                )))
            }
        }
    }
}
