//! Tx input resolution pipeline.
//!
//! Orchestrates three stages:
//! 1. **Narrow**: query the UTxO store to build a pool of candidate UTxOs
//! 2. **Approximate**: filter and rank candidates for each query independently
//! 3. **Assign**: allocate UTxOs across all queries simultaneously

use std::collections::BTreeMap;

use tx3_tir::encoding::AnyTir;
use tx3_tir::model::core::{UtxoRef, UtxoSet};

use crate::job::{InputResolutionJob, ResolveJob};
use crate::{Error, UtxoStore};

mod approximate;
pub(crate) mod assign;
mod canonical;
mod narrow;

#[cfg(test)]
pub(crate) mod test_utils;
#[cfg(test)]
mod tests;

pub use canonical::CanonicalQuery;

/// Resolve input queries against a UTxO store, mutating the
/// `InputResolutionJob` with results from each stage.
pub async fn resolve_queries<T: UtxoStore>(
    irj: &mut InputResolutionJob,
    utxos: &T,
) -> Result<BTreeMap<String, UtxoSet>, Error> {
    // 1. Narrow: build pool of candidate UTxOs from all queries
    narrow::build_utxo_pool(irj, utxos).await?;

    // 2. Approximate: rank candidates for each query independently
    approximate::approximate_queries(irj);

    // 3. Assign: allocate UTxOs across all queries
    assign::assign_all(irj);

    // 4. Validate: ensure all queries were resolved
    let pool_refs: Vec<UtxoRef> = irj
        .pool
        .as_ref()
        .map(|p| p.keys().cloned().collect())
        .unwrap_or_default();

    let mut all_inputs = BTreeMap::new();

    for entry in irj.assignments.as_ref().expect("assignments must be set") {
        if entry.selection.is_empty() {
            return Err(Error::InputNotResolved(
                entry.name.clone(),
                entry.query.clone(),
                pool_refs,
            ));
        }
        all_inputs.insert(entry.name.clone(), entry.selection.clone());
    }

    irj.resolved_inputs = Some(all_inputs.clone());

    Ok(all_inputs)
}

/// Resolve all input queries in a TIR transaction.
pub async fn resolve<T: UtxoStore>(
    tx: AnyTir,
    utxos: &T,
    job: &mut ResolveJob,
) -> Result<AnyTir, Error> {
    let mut queries: Vec<(String, CanonicalQuery)> = Vec::new();

    for (name, query) in tx3_tir::reduce::find_queries(&tx) {
        queries.push((name, CanonicalQuery::try_from(query)?));
    }

    let mut irj = InputResolutionJob::new(queries);
    let all_inputs = resolve_queries(&mut irj, utxos).await?;
    job.input_resolution = Some(irj);

    let out = tx3_tir::reduce::apply_inputs(tx, &all_inputs)?;

    Ok(out)
}
