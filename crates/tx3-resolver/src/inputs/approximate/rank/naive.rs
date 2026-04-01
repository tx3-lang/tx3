use tx3_tir::model::{
    assets::CanonicalAssets,
    core::{Utxo, UtxoSet},
};

use super::Rank;

#[allow(dead_code)]
pub struct NaiveRanker;

impl Rank for NaiveRanker {
    fn sorted_candidates(search_space: UtxoSet, _target: &CanonicalAssets) -> Vec<Utxo> {
        Vec::from_iter(search_space)
    }
}
