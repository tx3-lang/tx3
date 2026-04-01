//! Ranking strategies for UTxO candidates.
//!
//! A ranker sorts candidates by how well they match a target asset composition.
//! Different strategies trade off precision for simplicity.

use tx3_tir::model::{
    assets::CanonicalAssets,
    core::{Utxo, UtxoSet},
};

pub mod naive;
pub mod vector;

/// Rank candidates by relevance to a target asset composition.
pub trait Rank {
    fn sorted_candidates(search_space: UtxoSet, target: &CanonicalAssets) -> Vec<Utxo>;
}

#[cfg(not(feature = "naive_selector"))]
pub type Ranker = vector::VectorRanker;

#[cfg(feature = "naive_selector")]
pub type Ranker = naive::NaiveRanker;
