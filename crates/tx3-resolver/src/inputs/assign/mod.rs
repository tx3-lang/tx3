//! Assignment stage: given all queries with their ranked candidates, find an
//! allocation that satisfies everything simultaneously.

use std::collections::HashSet;

use tx3_tir::model::{
    assets::CanonicalAssets,
    core::{Utxo, UtxoRef, UtxoSet},
};

use crate::job::{InputResolutionJob, QueryResolution};

#[cfg(test)]
mod tests;

/// How tightly constrained a query is, from most to least specific.
/// Lower values get priority during assignment so that the most constrained
/// queries pick UTxOs first.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Specificity {
    SingleCollateral = 0,
    Single = 1,
    ManyCollateral = 2,
    Many = 3,
}

impl QueryResolution {
    fn specificity(&self) -> Specificity {
        match (self.query.support_many, self.query.collateral) {
            (false, true) => Specificity::SingleCollateral,
            (false, false) => Specificity::Single,
            (true, true) => Specificity::ManyCollateral,
            (true, false) => Specificity::Many,
        }
    }

    fn constraint_tightness(&self) -> (usize, Specificity, &str) {
        (self.candidates.len(), self.specificity(), &self.name)
    }

    fn pick(&mut self, used: &HashSet<UtxoRef>) {
        let available: Vec<Utxo> = self
            .candidates
            .drain(..)
            .filter(|utxo| !used.contains(&utxo.r#ref))
            .collect();

        let target = self
            .query
            .min_amount
            .clone()
            .unwrap_or(CanonicalAssets::empty());

        let selection = if self.query.support_many {
            pick_many(available, &target)
        } else {
            pick_single(available, &target)
        };

        self.selection = Some(selection);
    }
}

/// Select the first candidate that fully covers the target.
fn pick_single(candidates: Vec<Utxo>, target: &CanonicalAssets) -> UtxoSet {
    candidates
        .into_iter()
        .find(|utxo| utxo.assets.contains_total(target))
        .into_iter()
        .collect()
}

/// Accumulate candidates greedily until the target is fully covered,
/// then trim any excess UTxOs that aren't needed.
fn pick_many(candidates: Vec<Utxo>, target: &CanonicalAssets) -> UtxoSet {
    let mut matched = HashSet::new();
    let mut pending = target.clone();

    for candidate in candidates {
        if candidate.assets.contains_some(&pending) {
            matched.insert(candidate.clone());
            let to_include = candidate.assets.clone();
            pending = pending - to_include;
        }

        if pending.is_empty_or_negative() {
            break;
        }
    }

    if !pending.is_empty_or_negative() {
        return HashSet::new();
    }

    while let Some(utxo) = find_first_excess_utxo(&matched, target) {
        matched.remove(&utxo);
    }

    matched
}

fn find_first_excess_utxo(utxos: &HashSet<Utxo>, target: &CanonicalAssets) -> Option<Utxo> {
    if utxos.len() == 1 {
        return None;
    }

    let available = utxos
        .iter()
        .fold(CanonicalAssets::empty(), |acc, x| acc + x.assets.clone());

    let excess = available - target.clone();

    if excess.is_empty_or_negative() {
        return None;
    }

    for utxo in utxos.iter() {
        if excess.contains_total(&utxo.assets) {
            return Some(utxo.clone());
        }
    }

    None
}

impl InputResolutionJob {
    /// Given queries with their ranked candidates (from the approximation
    /// stage), find an allocation that satisfies all queries simultaneously
    /// using greedy assignment.
    pub fn assign_all(&mut self) {
        // Sort indices by constraint tightness so tightest queries pick first.
        let mut indices: Vec<usize> = (0..self.queries.len()).collect();
        indices.sort_by(|&a, &b| {
            self.queries[a]
                .constraint_tightness()
                .cmp(&self.queries[b].constraint_tightness())
        });

        let mut used: HashSet<UtxoRef> = HashSet::new();

        for idx in indices {
            self.queries[idx].pick(&used);

            if let Some(selection) = &self.queries[idx].selection {
                for utxo in selection.iter() {
                    used.insert(utxo.r#ref.clone());
                }
            }
        }
    }
}
