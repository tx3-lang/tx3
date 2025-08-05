use std::collections::HashSet;

use tx3_lang::{
    backend::{UtxoPattern, UtxoStore},
    ir, AssetClass, CanonicalAssets, UtxoRef,
};

use crate::inputs::CanonicalQuery;

use super::Error;

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

    fn intersection_of_all<const N: usize>(subsets: [Self; N]) -> Self {
        let mut result = Subset::All;

        for subset in subsets {
            result = Self::intersection(result, subset);
        }

        result
    }

    fn is_empty(&self) -> bool {
        match self {
            Self::All => false,
            Self::Specific(s) => s.is_empty(),
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
    pub matched: HashSet<UtxoRef>,
    pub by_address_count: Option<usize>,
    pub by_asset_class_count: Option<usize>,
    pub by_ref_count: Option<usize>,
}

impl std::fmt::Display for SearchSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SearchSpace {{")?;
        write!(f, "matched:{}, ", self.matched.len())?;

        for (i, ref_) in self.matched.iter().enumerate().take(10) {
            write!(f, "match[{}]: {}, ", i, ref_)?;
        }

        if self.matched.len() > 10 {
            write!(f, "... {} more, ", self.matched.len() - 10)?;
        }

        write!(f, "by_address_count: {:?}, ", self.by_address_count)?;
        write!(f, "by_asset_class_count: {:?}, ", self.by_asset_class_count)?;
        write!(f, "by_ref_count: {:?}, ", self.by_ref_count)?;
        write!(f, "}}")?;
        Ok(())
    }
}

async fn narrow_by_address<T: UtxoStore>(store: &T, address: &[u8]) -> Result<Subset, Error> {
    let utxos = store.narrow_refs(UtxoPattern::by_address(address)).await?;

    Ok(Subset::Specific(utxos.into_iter().collect()))
}

async fn narrow_by_asset_class<T: UtxoStore>(
    store: &T,
    assets: &AssetClass,
) -> Result<Subset, Error> {
    // skip filtering lovelace since it's not an custom asset
    if matches!(assets, AssetClass::Naked) {
        return Ok(Subset::All);
    }

    let AssetClass::Defined(policy, name) = assets else {
        return Ok(Subset::All);
    };

    let utxos = store
        .narrow_refs(UtxoPattern::by_asset(policy, name))
        .await?;

    Ok(Subset::Specific(utxos.into_iter().collect()))
}

async fn narrow_by_multi_asset_presence<T: UtxoStore>(
    store: &T,
    assets: &CanonicalAssets,
) -> Result<Subset, Error> {
    let mut matches = Subset::All;

    for (class, amount) in assets.iter() {
        if *amount > 0 {
            let next = narrow_by_asset_class(store, class).await?;
            matches = Subset::intersection(matches, next);
        }
    }

    Ok(matches)
}

pub async fn narrow_search_space<T: UtxoStore>(
    store: &T,
    criteria: &CanonicalQuery,
    max_size: usize,
) -> Result<SearchSpace, Error> {
    let matching_address = if let Some(address) = &criteria.address {
        narrow_by_address(store, address).await?
    } else {
        Subset::All
    };

    let matching_assets = if let Some(min_amount) = &criteria.min_amount {
        narrow_by_multi_asset_presence(store, min_amount).await?
    } else {
        Subset::All
    };

    let matching_refs = if !criteria.refs.is_empty() {
        Subset::Specific(criteria.refs.clone())
    } else {
        Subset::All
    };

    let by_address_count = matching_address.count();
    let by_asset_class_count = matching_assets.count();
    let by_ref_count = matching_refs.count();

    let subset = Subset::intersection_of_all([matching_address, matching_assets, matching_refs]);

    let mut refs = match subset {
        Subset::Specific(refs) => refs,
        Subset::All => return Err(Error::InputQueryTooBroad),
    };

    if refs.len() > max_size {
        let first_n = refs.into_iter().take(max_size);
        refs = HashSet::from_iter(first_n);
    }

    Ok(SearchSpace {
        matched: refs,
        by_address_count,
        by_asset_class_count,
        by_ref_count,
    })
}
