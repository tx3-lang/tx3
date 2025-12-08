use std::collections::HashSet;

use tx3_lang::{CanonicalAssets, Utxo, UtxoSet};

use super::CoinSelection;

#[allow(dead_code)]
pub struct NaiveSelector;

impl CoinSelection for NaiveSelector {
    fn pick_many(search_space: UtxoSet, target: &CanonicalAssets) -> HashSet<Utxo> {
        let mut matched = HashSet::new();
        let mut pending = target.clone();

        for utxo in search_space.iter() {
            if utxo.assets.contains_some(&pending) {
                matched.insert(utxo.clone());
                let to_include = utxo.assets.clone();
                pending = pending - to_include;
            }

            if pending.is_empty_or_negative() {
                // break early if we already have enough
                break;
            }
        }

        if !pending.is_empty_or_negative() {
            // if we didn't accumulate enough by the end of the search space,
            // then we didn't find a match
            return HashSet::new();
        }

        while let Some(utxo) = super::find_first_excess_utxo(&matched, target) {
            matched.remove(&utxo);
        }

        matched
    }

    fn pick_single(search_space: UtxoSet, target: &CanonicalAssets) -> UtxoSet {
        for utxo in search_space.iter() {
            if utxo.assets.contains_total(target) {
                return HashSet::from_iter(vec![utxo.clone()]);
            }
        }

        HashSet::new()
    }
}
