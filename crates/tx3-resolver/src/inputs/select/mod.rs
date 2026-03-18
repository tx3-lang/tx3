use std::collections::{BTreeMap, HashMap, HashSet};

use tx3_tir::model::{
    assets::CanonicalAssets,
    core::{Utxo, UtxoRef, UtxoSet},
};

use crate::{
    inputs::{narrow, CanonicalQuery},
    Error, UtxoStore,
};

pub mod naive;
pub mod vector;

#[cfg(test)]
mod tests;

pub trait CoinSelection {
    fn pick_many(search_space: UtxoSet, target: &CanonicalAssets) -> UtxoSet;
    fn pick_single(search_space: UtxoSet, target: &CanonicalAssets) -> UtxoSet;
}

pub fn find_first_excess_utxo(utxos: &HashSet<Utxo>, target: &CanonicalAssets) -> Option<Utxo> {
    // if there is only one utxo, then we can't remove them. This is to avoid the
    // edge case where the target is empty (eg: 0 fees gas on a testnet or L2)
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

#[cfg(not(feature = "naive_selector"))]
pub type Strategy = vector::VectorSelector;

#[cfg(feature = "naive_selector")]
pub type Strategy = naive::NaiveSelector;

/// How tightly constrained a query is, from most to least specific.
/// Lower values get priority during selection so that the most constrained
/// queries pick UTxOs first.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Specificity {
    SingleCollateral = 0,
    Single = 1,
    ManyCollateral = 2,
    Many = 3,
}

struct PendingQuery {
    name: String,
    query: CanonicalQuery,
    candidates: Vec<Utxo>,
}

impl PendingQuery {
    fn specificity(&self) -> Specificity {
        match (self.query.support_many, self.query.collateral) {
            (false, true) => Specificity::SingleCollateral,
            (false, false) => Specificity::Single,
            (true, true) => Specificity::ManyCollateral,
            (true, false) => Specificity::Many,
        }
    }

    fn build_candidates(&mut self, pool: &HashMap<UtxoRef, Utxo>) {
        let target = self
            .query
            .min_amount
            .clone()
            .unwrap_or(CanonicalAssets::empty());

        let pool_set: UtxoSet = pool
            .values()
            .filter(|utxo| {
                let collateral_ok = !self.query.collateral || utxo.assets.is_only_naked();
                let address_ok = self
                    .query
                    .address
                    .as_ref()
                    .map_or(true, |a| utxo.address == *a);
                let refs_ok = self.query.refs.is_empty() || self.query.refs.contains(&utxo.r#ref);
                collateral_ok && address_ok && refs_ok
            })
            .cloned()
            .collect();

        let ordered = vector::VectorSelector::sorted_candidates(pool_set, &target);

        self.candidates = if self.query.support_many {
            ordered
                .into_iter()
                .filter(|utxo| utxo.assets.contains_some(&target))
                .collect()
        } else {
            ordered
                .into_iter()
                .filter(|utxo| utxo.assets.contains_total(&target))
                .collect()
        };
    }

    fn constraint_tightness(&self) -> (usize, Specificity, &str) {
        (self.candidates.len(), self.specificity(), &self.name)
    }

    fn pick(self, used: &HashSet<UtxoRef>) -> (String, CanonicalQuery, UtxoSet) {
        let available: UtxoSet = self
            .candidates
            .into_iter()
            .filter(|utxo| !used.contains(&utxo.r#ref))
            .collect();

        let target = self
            .query
            .min_amount
            .clone()
            .unwrap_or(CanonicalAssets::empty());

        let selection = if self.query.support_many {
            Strategy::pick_many(available, &target)
        } else {
            Strategy::pick_single(available, &target)
        };

        (self.name, self.query, selection)
    }
}

pub struct InputSelector<'a, S: UtxoStore> {
    store: &'a S,
    pending: Vec<PendingQuery>,
    pool: HashMap<UtxoRef, Utxo>,
}

impl<'a, S: UtxoStore> InputSelector<'a, S> {
    pub fn new(store: &'a S) -> Self {
        Self {
            store,
            pending: Vec::new(),
            pool: HashMap::new(),
        }
    }

    pub async fn add(&mut self, name: String, query: CanonicalQuery) -> Result<(), Error> {
        let space = narrow::narrow_search_space(self.store, &query).await?;
        let refs = space.take(Some(super::MAX_SEARCH_SPACE_SIZE));
        let utxos = self.store.fetch_utxos(refs).await?;
        for utxo in utxos.iter() {
            self.pool
                .entry(utxo.r#ref.clone())
                .or_insert_with(|| utxo.clone());
        }
        self.pending.push(PendingQuery {
            name,
            query,
            candidates: Vec::new(),
        });
        Ok(())
    }

    pub fn select_all(mut self) -> Result<BTreeMap<String, UtxoSet>, Error> {
        let pool_refs: Vec<UtxoRef> = self.pool.keys().cloned().collect();

        for pq in &mut self.pending {
            pq.build_candidates(&self.pool);
        }

        self.pending
            .sort_by(|a, b| a.constraint_tightness().cmp(&b.constraint_tightness()));

        let mut assignments: BTreeMap<String, UtxoSet> = BTreeMap::new();
        let mut used: HashSet<UtxoRef> = HashSet::new();

        for pq in self.pending {
            let (name, query, selection) = pq.pick(&used);

            if selection.is_empty() {
                return Err(Error::InputNotResolved(name, query, pool_refs));
            }

            for utxo in selection.iter() {
                used.insert(utxo.r#ref.clone());
            }

            assignments.insert(name, selection);
        }

        Ok(assignments)
    }
}
