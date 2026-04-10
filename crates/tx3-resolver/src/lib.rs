use std::collections::HashSet;

use tx3_tir::compile::{CompiledTx, Compiler};
use tx3_tir::encoding::AnyTir;
use tx3_tir::model::v1beta0 as tir;
use tx3_tir::reduce::{Apply as _, ArgMap};
use tx3_tir::Node as _;

use crate::inputs::CanonicalQuery;
use crate::job::{ResolveJob, ResolveLog};

pub mod inputs;
pub mod interop;
pub mod job;
pub mod trp;

pub use tx3_tir::model::assets::CanonicalAssets;
pub use tx3_tir::model::core::{Type, Utxo, UtxoRef, UtxoSet};

// TODO: we need to re-export this because some of the UtxoStore interface depends ond them, but this is tech debt. We should remove any dependency to versioned IR artifacts.
pub use tx3_tir::model::v1beta0::{Expression, StructExpr};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("can't compile non-constant tir")]
    CantCompileNonConstantTir,

    #[error(transparent)]
    CompileError(#[from] tx3_tir::compile::Error),

    #[error(transparent)]
    InteropError(#[from] interop::Error),

    #[error(transparent)]
    ReduceError(#[from] tx3_tir::reduce::Error),

    #[error("expected {0}, got {1:?}")]
    ExpectedData(String, tir::Expression),

    #[error("input query too broad")]
    InputQueryTooBroad,

    #[error("input not resolved: {0}")]
    InputNotResolved(String, CanonicalQuery, Vec<UtxoRef>),

    #[error("missing argument `{key}` of type {ty:?}")]
    MissingTxArg {
        key: String,
        ty: tx3_tir::model::core::Type,
    },

    #[error("transient error: {0}")]
    TransientError(String),

    #[error("store error: {0}")]
    StoreError(String),

    #[error("TIR encode / decode error: {0}")]
    TirEncodingError(#[from] tx3_tir::encoding::Error),

    #[error("tx was not accepted: {0}")]
    TxNotAccepted(String),

    #[error("tx script returned failure")]
    TxScriptFailure(Vec<String>),
}

pub enum UtxoPattern<'a> {
    ByAddress(&'a [u8]),
    ByAssetPolicy(&'a [u8]),
    ByAsset(&'a [u8], &'a [u8]),
}

impl<'a> UtxoPattern<'a> {
    pub fn by_address(address: &'a [u8]) -> Self {
        Self::ByAddress(address)
    }

    pub fn by_asset_policy(policy: &'a [u8]) -> Self {
        Self::ByAssetPolicy(policy)
    }

    pub fn by_asset(policy: &'a [u8], name: &'a [u8]) -> Self {
        Self::ByAsset(policy, name)
    }
}

#[trait_variant::make(Send)]
pub trait UtxoStore {
    async fn narrow_refs(&self, pattern: UtxoPattern<'_>) -> Result<HashSet<UtxoRef>, Error>;
    async fn fetch_utxos(&self, refs: HashSet<UtxoRef>) -> Result<UtxoSet, Error>;
}

impl ResolveJob {
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
        let _ = job.dump_to_dir(std::path::Path::new(&dir));
    }

    result
}
