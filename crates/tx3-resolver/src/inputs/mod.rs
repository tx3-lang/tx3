//! Tx input resolution pipeline.
//!
//! Orchestrates three stages:
//! 1. **Narrow**: query the UTxO store to build a pool of candidate UTxOs
//! 2. **Approximate**: filter and rank candidates for each query independently
//! 3. **Assign**: allocate UTxOs across all queries simultaneously

use std::collections::BTreeMap;

use tx3_tir::encoding::AnyTir;
use tx3_tir::model::core::UtxoRef;

use crate::{Error, UtxoStore};

mod approximate;
mod assign;
mod canonical;
mod narrow;

pub use canonical::CanonicalQuery;

pub async fn resolve<T: UtxoStore>(tx: AnyTir, utxos: &T) -> Result<AnyTir, Error> {
    let mut queries: Vec<(String, CanonicalQuery)> = Vec::new();

    for (name, query) in tx3_tir::reduce::find_queries(&tx) {
        queries.push((name, CanonicalQuery::try_from(query)?));
    }

    // 1. Narrow: build pool of candidate UTxOs from all queries
    let pool = narrow::build_utxo_pool(utxos, &queries).await?;

    // 2. Approximate: rank candidates for each query independently
    let prepared = approximate::approximate_queries(&pool, queries);

    // 3. Assign: allocate UTxOs across all queries
    let assignments = assign::assign_all(prepared);

    // 4. Validate: ensure all queries were resolved
    let pool_refs: Vec<UtxoRef> = pool.keys().cloned().collect();
    let mut all_inputs = BTreeMap::new();

    for entry in assignments {
        if entry.selection.is_empty() {
            return Err(Error::InputNotResolved(entry.name, entry.query, pool_refs));
        }
        all_inputs.insert(entry.name, entry.selection);
    }

    let out = tx3_tir::reduce::apply_inputs(tx, &all_inputs)?;

    Ok(out)
}
