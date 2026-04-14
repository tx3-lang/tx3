//! Approximation stage: produces a ranked list of viable UTxO candidates for
//! each query independently, without cross-query awareness.

use std::collections::HashMap;

use tx3_tir::model::{
    assets::CanonicalAssets,
    core::{Utxo, UtxoRef},
};

use crate::inputs::canonical::CanonicalQuery;
use crate::job::ResolveJob;

pub mod filter;
pub mod rank;

use rank::{Rank as _, Ranker};

impl ResolveJob {
    /// For each query, filter the pool by hard constraints, rank by closeness
    /// to the target asset composition, then filter by aggregate constraints.
    pub fn approximate_queries(&mut self) {
        let pool = self
            .input_pool
            .as_ref()
            .expect("pool must be set before approximate");

        for qr in &mut self.input_queries {
            qr.candidates = approximate_candidates(pool, &qr.query);
        }
    }
}

fn approximate_candidates(pool: &HashMap<UtxoRef, Utxo>, query: &CanonicalQuery) -> Vec<Utxo> {
    let target = query.min_amount.clone().unwrap_or(CanonicalAssets::empty());

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
