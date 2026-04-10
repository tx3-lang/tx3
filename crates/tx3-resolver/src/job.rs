use std::collections::{BTreeMap, HashMap};
use std::path::Path;

use serde::{Deserialize, Serialize};
use tx3_tir::compile::CompiledTx;
use tx3_tir::encoding::AnyTir;
use tx3_tir::model::core::{Utxo, UtxoRef, UtxoSet};
use tx3_tir::reduce::ArgMap;

use crate::inputs::CanonicalQuery;
use crate::Error;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ResolveLog {
    ArgsApplied(AnyTir),
    FeesApplied(AnyTir),
    CompilerApplied(AnyTir),
    Reduced(AnyTir),
    InputsResolved(AnyTir),
    FinalReduced(AnyTir),
    Compiled(CompiledTx),
    Converged,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolveLogEntry {
    pub round: usize,
    pub event: ResolveLog,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolveJob {
    // Inputs (set once at creation)
    pub original_tir: AnyTir,
    pub args: ArgMap,

    // Pipeline state
    pub round: usize,
    pub last_eval: Option<CompiledTx>,
    pub converged: bool,

    // Timeline of state changes across all rounds
    pub log: Vec<ResolveLogEntry>,

    // Input resolution state (overwritten each eval pass)
    pub input_queries: Vec<QueryResolution>,
    pub input_pool: Option<HashMap<UtxoRef, Utxo>>,
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
    pub fn new(tx: AnyTir, args: ArgMap) -> Self {
        Self {
            original_tir: tx,
            args,
            round: 0,
            last_eval: None,
            converged: false,
            log: Vec::new(),
            input_queries: Vec::new(),
            input_pool: None,
        }
    }

    pub fn record(&mut self, event: ResolveLog) {
        self.log.push(ResolveLogEntry {
            round: self.round,
            event,
        });
    }

    pub fn resolved_tir(&self) -> &AnyTir {
        self.log
            .iter()
            .rev()
            .find_map(|entry| match &entry.event {
                ResolveLog::ArgsApplied(tir) => Some(tir),
                _ => None,
            })
            .expect("apply_args must be called before eval_pass")
    }

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

    pub fn dump_to_dir(&self, dir: &Path) -> Result<String, std::io::Error> {
        std::fs::create_dir_all(dir)?;
        let dump_id = uuid::Uuid::new_v4().to_string();
        let path = dir.join(format!("resolve-job-{dump_id}.json"));
        let file = std::fs::File::create(&path)?;
        serde_json::to_writer_pretty(file, self)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
        tracing::debug!(dump_id = %dump_id, path = %path.display(), "diagnostic dump written");
        Ok(dump_id)
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
