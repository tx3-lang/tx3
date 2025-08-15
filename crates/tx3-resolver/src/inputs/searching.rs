use std::collections::HashSet;

use tx3_lang::{
    backend::{UtxoPattern, UtxoStore},
    AssetClass, UtxoRef,
};

use crate::inputs::CanonicalQuery;

use super::Error;

#[derive(Debug, Clone)]
pub enum Subset {
    All,
    Specific(HashSet<UtxoRef>),
}

impl Subset {
    fn count(&self) -> Option<usize> {
        match self {
            Self::All => None,
            Self::Specific(s) => Some(s.len()),
        }
    }

    #[allow(dead_code)]
    fn union(a: Self, b: Self) -> Self {
        match (a, b) {
            (Self::All, _) => Self::All,
            (_, Self::All) => Self::All,
            (Self::Specific(s1), Self::Specific(s2)) => {
                Self::Specific(s1.union(&s2).cloned().collect())
            }
        }
    }

    fn intersection(a: Self, b: Self) -> Self {
        match (a, b) {
            (Self::All, x) => x,
            (x, Self::All) => x,
            (Self::Specific(s1), Self::Specific(s2)) => {
                Self::Specific(s1.intersection(&s2).cloned().collect())
            }
        }
    }
}

impl From<HashSet<UtxoRef>> for Subset {
    fn from(value: HashSet<UtxoRef>) -> Self {
        Self::Specific(value)
    }
}

#[derive(Debug, Clone)]
pub struct SearchSpace {
    pub union: HashSet<UtxoRef>,
    pub intersection: HashSet<UtxoRef>,
    pub by_address_count: Option<usize>,
    pub by_asset_class_count: Option<usize>,
    pub by_ref_count: Option<usize>,
}

impl SearchSpace {
    fn new() -> Self {
        Self {
            union: HashSet::new(),
            intersection: HashSet::new(),
            by_address_count: None,
            by_asset_class_count: None,
            by_ref_count: None,
        }
    }

    fn is_empty(&self) -> bool {
        self.union.is_empty()
    }

    fn add_matches(&mut self, utxos: HashSet<UtxoRef>) {
        self.union = self.union.union(&utxos).cloned().collect();
        self.intersection = self.intersection.intersection(&utxos).cloned().collect();
    }

    fn add_address_matches(&mut self, subset: Subset) {
        match subset {
            Subset::All => (),
            Subset::Specific(utxos) => {
                *self.by_address_count.get_or_insert(0) += utxos.len();
                self.add_matches(utxos);
            }
        }
    }

    fn add_asset_class_matches(&mut self, subset: Subset) {
        match subset {
            Subset::All => (),
            Subset::Specific(utxos) => {
                *self.by_asset_class_count.get_or_insert(0) += utxos.len();
                self.add_matches(utxos);
            }
        }
    }

    fn add_ref_matches(&mut self, utxos: HashSet<UtxoRef>) {
        *self.by_ref_count.get_or_insert(0) += utxos.len();
        self.add_matches(utxos);
    }

    pub fn take(&self, take: Option<usize>) -> HashSet<UtxoRef> {
        let Some(take) = take else {
            return self.union.clone();
        };

        // first we take from the intersection which are the best matches
        let best: HashSet<_> = self.intersection.iter().take(take).cloned().collect();

        if best.len() < take {
            let others: HashSet<_> = self.union.difference(&best).cloned().collect();
            let remaining: HashSet<_> = others.into_iter().take(take - best.len()).collect();
            best.union(&remaining).cloned().collect()
        } else {
            best
        }
    }
}

async fn narrow_by_asset_class<T: UtxoStore>(
    store: &T,
    parent: Subset,
    class: &AssetClass,
) -> Result<Subset, Error> {
    // skip filtering lovelace since it's not an custom asset
    if matches!(class, AssetClass::Naked) {
        return Ok(Subset::All);
    }

    let AssetClass::Defined(policy, name) = class else {
        return Ok(Subset::All);
    };

    let utxos = store
        .narrow_refs(UtxoPattern::by_asset(policy, name))
        .await?;

    Ok(Subset::intersection(parent, Subset::Specific(utxos)))
}

pub async fn narrow_search_space<T: UtxoStore>(
    store: &T,
    criteria: &CanonicalQuery,
) -> Result<SearchSpace, Error> {
    let mut search_space = SearchSpace::new();

    let parent_subset = if let Some(address) = &criteria.address {
        let utxos = store.narrow_refs(UtxoPattern::by_address(address)).await?;
        Subset::Specific(utxos)
    } else {
        Subset::All
    };

    search_space.add_address_matches(parent_subset.clone());

    if let Some(assets) = &criteria.min_amount {
        for (class, amount) in assets.iter() {
            if *amount > 0 {
                let subset = narrow_by_asset_class(store, parent_subset.clone(), class).await?;
                search_space.add_asset_class_matches(subset);
            }
        }
    }

    if !criteria.refs.is_empty() {
        search_space.add_ref_matches(criteria.refs.clone());
    }

    if search_space.is_empty() {
        return Err(Error::InputQueryTooBroad);
    }

    Ok(search_space)
}
