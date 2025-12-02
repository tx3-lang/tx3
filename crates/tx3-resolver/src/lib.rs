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

pub trait UtxoMempool {
    fn lock(&self, refs: &[UtxoRef]) -> bool;
    fn unlock(&self, refs: &[UtxoRef]);
    fn register_outputs(&self, tx_bytes: &[u8]);
}

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
) -> Result<Option<(TxEval, HashSet<UtxoRef>)>, Error> {
    let attempt = tx.clone();

    let fees = last_eval.as_ref().map(|e| e.fee).unwrap_or(0);

    let attempt = applying::apply_fees(attempt, fees)?;

    let attempt = attempt.apply(compiler)?;

    let attempt = applying::reduce(attempt)?;

    let (attempt, refs) = crate::inputs::resolve(attempt, utxos).await?;

    let attempt = tx3_lang::ProtoTx::from(attempt);

    let attempt = attempt.apply()?;

    if !attempt.as_ref().is_constant() {
        return Err(Error::CantCompileNonConstantTir);
    }

    let eval = compiler.compile(attempt.as_ref())?;

    let Some(last_eval) = last_eval else {
        return Ok(Some((eval, refs)));
    };

    if eval != *last_eval {
        return Ok(Some((eval, refs)));
    }

    Ok(None)
}

pub async fn resolve_tx<C: Compiler, S: UtxoStore, M: UtxoMempool>(
    tx: tx3_lang::ProtoTx,
    compiler: &mut C,
    utxos: &S,
    mempool: &M,
    max_optimize_rounds: usize,
) -> Result<TxEval, Error> {
    let max_optimize_rounds = max_optimize_rounds.max(3);

    let mut last_eval = None;
    let mut rounds = 0;
    let mut locked_refs: Vec<UtxoRef> = vec![];
    let mut lock_retries = 0;
    const MAX_LOCK_RETRIES: usize = 20;

    // one initial pass to reduce any available params;
    let tx = tx.apply()?;

    // reduce compiler ops
    let tx = ir::Tx::from(tx);

    loop {
        let current_eval = last_eval.as_ref().map(|(e, _)| e);

        match eval_pass(&tx, compiler, utxos, current_eval).await? {
            Some((better, refs)) => {
                let new_refs_vec: Vec<_> = refs.iter().cloned().collect();

                // We need to filter out UTXOs that we already locked.
                let new_refs_set: HashSet<_> = refs.iter().collect();
                let current_locked_set: HashSet<_> = locked_refs.iter().collect();
                
                let to_lock: Vec<_> = refs.iter()
                    .filter(|r| !current_locked_set.contains(r))
                    .cloned().collect();
                    
                let to_unlock: Vec<_> = locked_refs.iter()
                    .filter(|r| !new_refs_set.contains(r))
                    .cloned().collect();

                let lock_success = if to_lock.is_empty() {
                    true
                } else {
                    mempool.lock(&to_lock)
                };

                if lock_success {
                    if !to_unlock.is_empty() {
                        mempool.unlock(&to_unlock);
                    }
                    
                    locked_refs = new_refs_vec;
                    last_eval = Some((better, refs));
                    lock_retries = 0;

                    if rounds > max_optimize_rounds {
                        break;
                    }

                    rounds += 1;
                } else {
                    // If we failed to lock, it means someone else took the new candidates.
                    // We can't use this solution.
                    // We should retry.
                    lock_retries += 1;
                    if lock_retries > MAX_LOCK_RETRIES {
                        if last_eval.is_some() {
                            break;
                        } else {
                            return Err(Error::BackendError(tx3_lang::backend::Error::StoreError(
                                "High contention: failed to lock UTXOs".into(),
                            )));
                        }
                    }
                }
            }
            None => {
                // Optimization finished.
                // We might have some unused locks if the last pass didn't use all of them?
                // No, locked_refs tracks exactly what was used in last_eval.
                break;
            }
        }
    }

    let (eval, _) = last_eval.unwrap();

    mempool.register_outputs(&eval.payload);

    Ok(eval)
}
