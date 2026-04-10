use std::collections::{BTreeMap, HashMap};

use serde::{Deserialize, Serialize};
use tx3_tir::compile::CompiledTx;
use tx3_tir::encoding::AnyTir;
use tx3_tir::model::core::{Utxo, UtxoRef, UtxoSet};
use tx3_tir::reduce::ArgMap;

use crate::inputs::assign::{Assignment, PreparedQuery};
use crate::inputs::CanonicalQuery;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolveJob {
    // Inputs (set once at creation)
    pub original_tir: AnyTir,
    pub args: ArgMap,
    pub max_optimize_rounds: usize,

    // Pipeline state
    pub resolved_tir: Option<AnyTir>,
    pub round: usize,
    pub last_eval: Option<CompiledTx>,
    pub converged: bool,

    // Per-round intermediates (overwritten each eval pass)
    pub after_fees: Option<AnyTir>,
    pub after_compile: Option<AnyTir>,
    pub after_reduce: Option<AnyTir>,
    pub input_resolution: Option<InputResolutionJob>,
    pub after_inputs: Option<AnyTir>,
    pub after_final_reduce: Option<AnyTir>,
}

impl ResolveJob {
    pub fn new(tx: AnyTir, args: ArgMap, max_optimize_rounds: usize) -> Self {
        Self {
            original_tir: tx,
            args,
            max_optimize_rounds: max_optimize_rounds.max(3),
            resolved_tir: None,
            round: 0,
            last_eval: None,
            converged: false,
            after_fees: None,
            after_compile: None,
            after_reduce: None,
            input_resolution: None,
            after_inputs: None,
            after_final_reduce: None,
        }
    }

    pub fn clear_round_state(&mut self) {
        self.after_fees = None;
        self.after_compile = None;
        self.after_reduce = None;
        self.input_resolution = None;
        self.after_inputs = None;
        self.after_final_reduce = None;
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InputResolutionJob {
    pub queries: Vec<(String, CanonicalQuery)>,
    pub pool: Option<HashMap<UtxoRef, Utxo>>,
    #[serde(skip)]
    pub prepared: Option<Vec<PreparedQuery>>,
    pub assignments: Option<Vec<Assignment>>,
    pub resolved_inputs: Option<BTreeMap<String, UtxoSet>>,
}

impl InputResolutionJob {
    pub fn new(queries: Vec<(String, CanonicalQuery)>) -> Self {
        Self {
            queries,
            pool: None,
            prepared: None,
            assignments: None,
            resolved_inputs: None,
        }
    }
}
