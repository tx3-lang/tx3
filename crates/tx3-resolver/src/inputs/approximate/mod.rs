//! Approximation stage: produces a ranked list of viable UTxO candidates for
//! each query independently, without cross-query awareness.

use std::collections::HashMap;

use tx3_tir::model::{
    assets::CanonicalAssets,
    core::{Utxo, UtxoRef},
};

use crate::inputs::{assign::PreparedQuery, canonical::CanonicalQuery};
use crate::job::InputResolutionJob;

pub mod filter;
pub mod rank;

use rank::{Rank as _, Ranker};

/// For each query, filter the pool by hard constraints, rank by closeness
/// to the target asset composition, then filter by aggregate constraints.
/// Writes `PreparedQuery` results into the `InputResolutionJob`.
pub fn approximate_queries(irj: &mut InputResolutionJob) {
    let pool = irj.pool.as_ref().expect("pool must be set before approximate");

    let prepared = irj
        .queries
        .iter()
        .map(|(name, query)| {
            let candidates = approximate_candidates(pool, query);
            PreparedQuery {
                name: name.clone(),
                query: query.clone(),
                candidates,
            }
        })
        .collect();

    irj.prepared = Some(prepared);
}

fn approximate_candidates(
    pool: &HashMap<UtxoRef, Utxo>,
    query: &CanonicalQuery,
) -> Vec<Utxo> {
    let target = query
        .min_amount
        .clone()
        .unwrap_or(CanonicalAssets::empty());

    let pool_set = pool
        .values()
        .filter(|utxo| filter::matches_hard_constraints(query, utxo))
        .cloned()
        .collect();

    let ordered = Ranker::sorted_candidates(pool_set, &target);

    ordered
        .into_iter()
        .filter(|utxo| filter::matches_aggregate_constraints(query, utxo, &target))
        .collect()
}
