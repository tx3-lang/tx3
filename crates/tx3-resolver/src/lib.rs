use std::collections::HashSet;
use tx3_lang::{
    applying::{self, Apply as _},
    backend::{self, Compiler, TxEval, UtxoStore},
    ir::{self, Node},
    UtxoRef,
};

pub mod inputs;

#[cfg(test)]
pub mod mock;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    InputsError(#[from] inputs::Error),

    #[error("can't compile non-constant tir")]
    CantCompileNonConstantTir,

    #[error("backend error: {0}")]
    BackendError(#[from] backend::Error),

    #[error("apply error: {0}")]
    ApplyError(#[from] applying::Error),
}

async fn eval_pass<C: Compiler, S: UtxoStore>(
    tx: &ir::Tx,
    compiler: &mut C,
    utxos: &S,
    last_eval: Option<&TxEval>,
) -> Result<Option<TxEval>, Error> {
    let attempt = tx.clone();

    let fees = last_eval.as_ref().map(|e| e.fee).unwrap_or(0);

    let attempt = applying::apply_fees(attempt, fees)?;

    let attempt = attempt.apply(compiler)?;

    let attempt = applying::reduce(attempt)?;

    let attempt = crate::inputs::resolve(attempt, utxos).await?;

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
    let max_optimize_rounds = max_optimize_rounds.max(3);

    let mut last_eval = None;
    let mut rounds = 0;

    // one initial pass to reduce any available params;
    let tx = tx.apply()?;

    // reduce compiler ops
    let tx = ir::Tx::from(tx);

    while let Some(better) = eval_pass(&tx, compiler, utxos, last_eval.as_ref()).await? {
        last_eval = Some(better);

        if rounds > max_optimize_rounds {
            break;
        }

        rounds += 1;
    }

    Ok(last_eval.unwrap())
}
