use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use tx3_tir::compile::CompiledTx;
use tx3_tir::encoding::AnyTir;
use tx3_tir::model::core::{Utxo, UtxoRef, UtxoSet};
use tx3_tir::reduce::ArgMap;

use crate::inputs::CanonicalQuery;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolveJob {
    // Inputs (set once at creation)
    pub original_tir: AnyTir,
    pub args: ArgMap,

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
    pub fn new(tx: AnyTir, args: ArgMap) -> Self {
        Self {
            original_tir: tx,
            args,
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

/// Tracks a single input query as it progresses through the resolution
/// pipeline: narrow → approximate (candidates) → assign (selection).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QueryResolution {
    pub name: String,
    pub query: CanonicalQuery,
    /// Ranked candidate UTxOs, set by the approximate stage.
    #[serde(skip)]
    pub candidates: Vec<Utxo>,
    /// Final UTxO selection, set by the assign stage.
    pub selection: Option<UtxoSet>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InputResolutionJob {
    pub queries: Vec<QueryResolution>,
    pub pool: Option<HashMap<UtxoRef, Utxo>>,
}

impl InputResolutionJob {
    pub fn new(queries: Vec<(String, CanonicalQuery)>) -> Self {
        Self {
            queries: queries
                .into_iter()
                .map(|(name, query)| QueryResolution {
                    name,
                    query,
                    candidates: Vec::new(),
                    selection: None,
                })
                .collect(),
            pool: None,
        }
    }
}
