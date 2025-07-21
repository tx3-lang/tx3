use tx3_lang::{
    applying::{self, Apply as _},
    backends::{self, Compiler, TxEval, UtxoStore},
    ir::{self, Node},
};

pub mod inputs;

#[cfg(test)]
pub mod mock;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("store error: {0}")]
    StoreError(String),

    #[error("input query too broad")]
    InputQueryTooBroad,

    #[error("input not resolved: {0} {1:?}")]
    InputNotResolved(String, ir::InputQuery),

    #[error("expected {0}, got {1:?}")]
    ExpectedData(String, ir::Expression),

    #[error("apply error: {0}")]
    ApplyError(#[from] applying::Error),

    #[error("max optimize rounds reached")]
    MaxOptimizeRoundsReached,

    #[error("can't compile non-constant tir")]
    CantCompileNonConstantTir,

    #[error("backend error: {0}")]
    BackendError(#[from] backends::Error),
}

async fn eval_pass<C: Compiler, S: UtxoStore>(
    tx: &ir::Tx,
    compiler: &C,
    utxos: &S,
    last_eval: Option<&TxEval>,
) -> Result<Option<TxEval>, Error> {
    let attempt = tx.clone();

    let fees = last_eval.as_ref().map(|e| e.fee).unwrap_or(0);
    let attempt = applying::apply_fees(attempt, fees)?;

    let attempt = applying::reduce(attempt)?;

    let attempt = crate::inputs::resolve(attempt.into(), utxos).await?;

    let attempt = tx3_lang::ProtoTx::from(attempt);

    let attempt = attempt.apply()?;

    if !attempt.as_ref().is_constant() {
        return Err(Error::CantCompileNonConstantTir);
    }

    let eval = compiler.compile(attempt.as_ref())?;

    let Some(last_eval) = last_eval else {
        return Ok(Some(eval));
    };

    if eval != *last_eval {
        return Ok(Some(eval));
    }

    Ok(None)
}

pub async fn resolve_tx<C: Compiler, S: UtxoStore>(
    tx: tx3_lang::ProtoTx,
    compiler: &mut C,
    utxos: &S,
    max_optimize_rounds: usize,
) -> Result<TxEval, Error> {
    let mut last_eval = None;
    let mut rounds = 0;

    // one initial pass to reduce any available params;
    let tx = tx.apply()?;

    // reduce compiler ops
    let tx = ir::Tx::from(tx);
    let tx = tx.apply(compiler)?;

    while let Some(better) = eval_pass(&tx, compiler, utxos, last_eval.as_ref()).await? {
        last_eval = Some(better);

        if rounds > max_optimize_rounds {
            return Err(Error::MaxOptimizeRoundsReached);
        }

        rounds += 1;
    }

    Ok(last_eval.unwrap())
}
