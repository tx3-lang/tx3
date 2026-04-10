use std::collections::{BTreeMap, HashMap};
use std::path::Path;

use serde::{Deserialize, Serialize};
use tx3_tir::compile::{CompiledTx, Compiler};
use tx3_tir::encoding::AnyTir;
use tx3_tir::model::core::{Utxo, UtxoRef, UtxoSet};
use tx3_tir::model::v1beta0 as tir;
use tx3_tir::reduce::{Apply as _, ArgMap};
use tx3_tir::Node as _;

use crate::inputs::CanonicalQuery;
use crate::{Error, UtxoStore};

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

    fn apply_args(&mut self) -> Result<(), Error> {
        let params = tx3_tir::reduce::find_params(&self.original_tir);

        for (key, ty) in params.iter() {
            if !self.args.contains_key(key) {
                return Err(Error::MissingTxArg {
                    key: key.to_string(),
                    ty: ty.clone(),
                });
            };
        }

        let tir = tx3_tir::reduce::apply_args(self.original_tir.clone(), &self.args)?;
        self.record(ResolveLog::ArgsApplied(tir));

        Ok(())
    }

    async fn eval_pass<C, S>(&mut self, compiler: &mut C, utxos: &S) -> Result<(), Error>
    where
        C: Compiler<Expression = tir::Expression, CompilerOp = tir::CompilerOp>,
        S: UtxoStore,
    {
        let base_tir = self.resolved_tir().clone();
        let fees = self.last_eval.as_ref().map(|e| e.fee).unwrap_or(0);

        let attempt = tx3_tir::reduce::apply_fees(base_tir, fees)?;
        self.record(ResolveLog::FeesApplied(attempt.clone()));

        let attempt = attempt.apply(compiler)?;
        self.record(ResolveLog::CompilerApplied(attempt.clone()));

        let attempt = tx3_tir::reduce::reduce(attempt)?;
        self.record(ResolveLog::Reduced(attempt.clone()));

        let attempt = self.resolve_inputs(attempt, utxos).await?;
        self.record(ResolveLog::InputsResolved(attempt.clone()));

        let attempt = tx3_tir::reduce::reduce(attempt)?;
        self.record(ResolveLog::FinalReduced(attempt.clone()));

        if !attempt.is_constant() {
            return Err(Error::CantCompileNonConstantTir);
        }

        let eval = compiler.compile(&attempt)?;

        let converged = self.last_eval.as_ref().map_or(false, |prev| eval == *prev);

        self.record(ResolveLog::Compiled(eval.clone()));
        self.last_eval = Some(eval);

        if converged {
            self.converged = true;
            self.record(ResolveLog::Converged);
        }

        self.round += 1;

        Ok(())
    }

    pub async fn execute<C, S>(
        &mut self,
        compiler: &mut C,
        utxos: &S,
        max_optimize_rounds: usize,
    ) -> Result<CompiledTx, Error>
    where
        C: Compiler<Expression = tir::Expression, CompilerOp = tir::CompilerOp>,
        S: UtxoStore,
    {
        let max_optimize_rounds = max_optimize_rounds.max(3);

        self.apply_args()?;

        loop {
            self.eval_pass(compiler, utxos).await?;

            if self.converged || self.round > max_optimize_rounds {
                break;
            }
        }

        Ok(self.last_eval.clone().unwrap())
    }
}

pub async fn resolve_tx<C, S>(
    tx: AnyTir,
    args: &ArgMap,
    compiler: &mut C,
    utxos: &S,
    max_optimize_rounds: usize,
) -> Result<CompiledTx, Error>
where
    C: Compiler<Expression = tir::Expression, CompilerOp = tir::CompilerOp>,
    S: UtxoStore,
{
    let mut job = ResolveJob::new(tx, args.clone());

    let result = job.execute(compiler, utxos, max_optimize_rounds).await;

    if let Ok(dir) = std::env::var("TX3_DIAGNOSTIC_DUMP") {
        let _ = job.dump_to_dir(Path::new(&dir));
    }

    result
}
