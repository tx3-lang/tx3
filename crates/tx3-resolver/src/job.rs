use std::collections::{BTreeMap, HashMap};

use serde::{Deserialize, Serialize};
use tx3_tir::compile::CompiledTx;
use tx3_tir::encoding::AnyTir;
use tx3_tir::model::core::{Utxo, UtxoRef, UtxoSet};
use tx3_tir::reduce::ArgMap;

use crate::inputs::CanonicalQuery;
use crate::Error;

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
    pub after_inputs: Option<AnyTir>,
    pub after_final_reduce: Option<AnyTir>,

    // Input resolution state (overwritten each eval pass)
    pub input_queries: Vec<QueryResolution>,
    pub input_pool: Option<HashMap<UtxoRef, Utxo>>,
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
            after_inputs: None,
            after_final_reduce: None,
            input_queries: Vec::new(),
            input_pool: None,
        }
    }

    pub fn clear_round_state(&mut self) {
        self.after_fees = None;
        self.after_compile = None;
        self.after_reduce = None;
        self.after_inputs = None;
        self.after_final_reduce = None;
        self.input_queries = Vec::new();
        self.input_pool = None;
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

impl QueryResolution {
    pub fn ensure_resolved(&self, pool_refs: &[UtxoRef]) -> Result<(), Error> {
        match &self.selection {
            Some(sel) if !sel.is_empty() => Ok(()),
            _ => Err(Error::InputNotResolved(
                self.name.clone(),
                self.query.clone(),
                pool_refs.to_vec(),
            )),
        }
    }
}

impl ResolveJob {
    pub fn set_input_queries(&mut self, queries: Vec<(String, CanonicalQuery)>) {
        self.input_queries = queries
            .into_iter()
            .map(|(name, query)| QueryResolution {
                name,
                query,
                candidates: Vec::new(),
                selection: None,
            })
            .collect();
    }

    pub fn pool_refs(&self) -> Vec<UtxoRef> {
        self.input_pool
            .as_ref()
            .map(|p| p.keys().cloned().collect())
            .unwrap_or_default()
    }

    pub fn to_input_map(&self) -> BTreeMap<String, UtxoSet> {
        self.input_queries
            .iter()
            .filter_map(|qr| {
                qr.selection
                    .as_ref()
                    .map(|sel| (qr.name.clone(), sel.clone()))
            })
            .collect()
    }
}
