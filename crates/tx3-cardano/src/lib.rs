use std::collections::HashMap;

pub mod coercion;
pub mod compile;

// Re-export pallas for upstream users
pub use pallas;

use pallas::ledger::{
    primitives::{self, RationalNumber},
    traverse::{ComputeHash, MultiEraTx},
    validate::phase2::{
        evaluate_tx,
        EvalReport,
        script_context::SlotConfig,
    },
};

use tx3_lang::ir;
use tx3_lang::UtxoRef;
use tx3_lang::backend::{UtxoStore, TxEval, CompiledTx};
use dolos_cardano::{PParamsSet, utils::pparams_to_pallas};

#[cfg(test)]
pub mod tests;

pub type Network = pallas::ledger::primitives::NetworkId;
pub type PlutusVersion = u8;
pub type CostModel = Vec<i64>;
pub type CostModels = HashMap<PlutusVersion, CostModel>;

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
}

pub struct Compiler {
    pub config: Config,
    pub network: Network,
    pub cursor: ChainPoint,
    pub pparams: PParamsSet,
    pub cost_models: CostModels,
    pub slot_config: SlotConfig,
    pub latest_tx_body: Option<TxBody>,
}

impl Compiler {
    pub fn new(
        config: Config,
        network: Network,
        cursor: ChainPoint,
        pparams: PParamsSet,
        cost_models: CostModels,
        slot_config: SlotConfig,
    ) -> Self {
        Self {
            config,
            network,
            cursor,
            pparams,
            cost_models,
            slot_config,
            latest_tx_body: None,
        }
    }
}

impl tx3_lang::backend::Compiler for Compiler {
    fn compile(&mut self, tx: &tx3_lang::ir::Tx) -> Result<CompiledTx, tx3_lang::backend::Error> {
        let compiled_tx = compile::entry_point(tx, &self.network, &self.cost_models)?;

        // evaluate redeemers and apply exunits here

        let hash = compiled_tx.transaction_body.compute_hash();
        let payload = pallas::codec::minicbor::to_vec(&compiled_tx).unwrap();

        self.latest_tx_body = Some(compiled_tx.transaction_body);

        Ok(CompiledTx {
            payload,
            hash: hash.to_vec()
        })
    }

    async fn evaluate<S: UtxoStore>(&self, tx: Option<&CompiledTx>, utxos: &S) -> Result<TxEval, tx3_lang::backend::Error> {
        
        if tx.is_none() {
            return Ok(TxEval {
                report: vec![],
                fee: 0,
            });
        }

        let payload  = &tx.unwrap().payload;

        let multi_era_tx = MultiEraTx::decode(payload)
            .map_err(|e| tx3_lang::backend::Error::EvaluationError(e.to_string()))?;

        let utxo_deps = utxos.fetch_utxos_deps(
            multi_era_tx
                .requires()
                .iter()
                .map(|r| UtxoRef {
                    txid: r.hash().to_vec(),
                    index: r.index() as u32
                })
                .collect(),
        ).await?;

        let report =
            evaluate_tx(&multi_era_tx, &pparams_to_pallas(&self.pparams), &utxo_deps, &self.slot_config)
                .map_err(|e| tx3_lang::backend::Error::EvaluationError(e.to_string()))?;

        let size_fees = eval_size_fees(payload, &self.pparams, self.config.extra_fees);
        let redeemers_fees = eval_redeemers_fees(&report, &self.pparams);

        Ok(TxEval {
            report,
            fee: size_fees + redeemers_fees,
        })
    }

    fn execute(&self, op: ir::CompilerOp) -> Result<ir::Expression, tx3_lang::backend::Error> {
        match op {
            ir::CompilerOp::BuildScriptAddress(x) => {
                let hash: primitives::Hash<28> = coercion::expr_into_hash(&x)?;
                let address = coercion::policy_into_address(hash.as_ref(), self.network)?;
                Ok(ir::Expression::Address(address.to_vec()))
            }
            ir::CompilerOp::ComputeMinUtxo(x) => {
                let lovelace = compile::compute_min_utxo(
                    x,
                    &self.latest_tx_body,
                    self.pparams.ada_per_utxo_byte_or_default() as i128,
                )?;
                Ok(ir::Expression::Assets(vec![ir::AssetExpr {
                    policy: ir::Expression::None,
                    asset_name: ir::Expression::None,
                    amount: ir::Expression::Number(lovelace),
                }]))
            }
            ir::CompilerOp::ComputeTipSlot => Ok(ir::Expression::Number(self.cursor.slot as i128)),
        }
    }
}

fn eval_size_fees(tx: &[u8], pparams: &PParamsSet, extra_fees: Option<u64>) -> u64 {
    tx.len() as u64 * pparams.min_fee_a_or_default() as u64
        + pparams.min_fee_b_or_default() as u64
        + extra_fees.unwrap_or(DEFAULT_EXTRA_FEES)
}

fn eval_redeemers_fees(report: &EvalReport, pparams: &PParamsSet) -> u64 {
    let primitives::ExUnitPrices { mem_price, step_price } = pparams.execution_costs()
        .unwrap_or(primitives::ExUnitPrices {
            mem_price: RationalNumber { numerator: 0, denominator: 1 },
            step_price: RationalNumber { numerator: 0, denominator: 1 },
        });

    let mut fees = 0;

    for r in report {
        fees += (r.units.mem * mem_price.numerator * step_price.denominator
            + r.units.steps * step_price.numerator * mem_price.denominator)
            / (mem_price.denominator * step_price.denominator);
    }

    fees
}
