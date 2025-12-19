use std::collections::HashSet;

use tx3_tir::compile::{CompiledTx, Compiler};
use tx3_tir::model::v1beta0 as tir;
use tx3_tir::reduce::Apply as _;
use tx3_tir::Node as _;

use crate::inputs::CanonicalQuery;

pub mod inputs;

#[cfg(test)]
pub mod mock;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("can't compile non-constant tir")]
    CantCompileNonConstantTir,

    #[error("compile error: {0}")]
    CompileError(Box<tx3_tir::compile::Error>),

    #[error("reduce error: {0}")]
    ReduceError(Box<tx3_tir::reduce::Error>),

    #[error("expected {0}, got {1:?}")]
    ExpectedData(String, tir::Expression),

    #[error("input query too broad")]
    InputQueryTooBroad,

    #[error("input not resolved: {0}")]
    InputNotResolved(String, CanonicalQuery, inputs::SearchSpace),

    #[error("transient error: {0}")]
    TransientError(String),

    #[error("store error: {0}")]
    StoreError(String),
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
    async fn narrow_refs(&self, pattern: UtxoPattern<'_>) -> Result<HashSet<tir::UtxoRef>, Error>;
    async fn fetch_utxos(&self, refs: HashSet<tir::UtxoRef>) -> Result<tir::UtxoSet, Error>;
}

impl From<tx3_tir::compile::Error> for Error {
    fn from(error: tx3_tir::compile::Error) -> Self {
        Error::CompileError(Box::new(error))
    }
}

impl From<tx3_tir::reduce::Error> for Error {
    fn from(error: tx3_tir::reduce::Error) -> Self {
        Error::ReduceError(Box::new(error))
    }
}

async fn eval_pass<C, S>(
    tx: &tir::Tx,
    compiler: &mut C,
    utxos: &S,
    last_eval: Option<&CompiledTx>,
) -> Result<Option<CompiledTx>, Error>
where
    C: Compiler<Expression = tir::Expression, CompilerOp = tir::CompilerOp, EntryPoint = tir::Tx>,
    S: UtxoStore,
{
    let attempt = tx.clone();

    let fees = last_eval.as_ref().map(|e| e.fee).unwrap_or(0);

    let attempt = tx3_tir::reduce::apply_fees(attempt, fees)?;

    let attempt = attempt.apply(compiler)?;

    let attempt = tx3_tir::reduce::reduce(attempt)?;

    let attempt = crate::inputs::resolve(attempt, utxos).await?;

    let attempt = tx3_tir::reduce::reduce(attempt)?;

    if !attempt.is_constant() {
        return Err(Error::CantCompileNonConstantTir);
    }

    let eval = compiler.compile(&attempt)?;

    let Some(last_eval) = last_eval else {
        return Ok(Some(eval));
    };

    if eval != *last_eval {
        return Ok(Some(eval));
    }

    Ok(None)
}

pub async fn resolve_tx<C, S>(
    tx: tir::Tx,
    compiler: &mut C,
    utxos: &S,
    max_optimize_rounds: usize,
) -> Result<CompiledTx, Error>
where
    C: Compiler<Expression = tir::Expression, CompilerOp = tir::CompilerOp, EntryPoint = tir::Tx>,
    S: UtxoStore,
{
    let max_optimize_rounds = max_optimize_rounds.max(3);

    let mut last_eval = None;
    let mut rounds = 0;

    while let Some(better) = eval_pass(&tx, compiler, utxos, last_eval.as_ref()).await? {
        last_eval = Some(better);

        if rounds > max_optimize_rounds {
            break;
        }

        rounds += 1;
    }

    Ok(last_eval.unwrap())
}
