use std::collections::HashSet;

use tx3_tir::model::{assets::AssetClass, v1beta0::UtxoRef};

use crate::{inputs::CanonicalQuery, UtxoPattern, UtxoStore};

use super::Error;

#[derive(Debug, Clone)]
pub enum Subset {
    NotSet,
    All,
    Specific(HashSet<UtxoRef>),
}

impl Subset {
    fn count(&self) -> Option<usize> {
        match self {
            Self::NotSet => None,
            Self::All => None,
            Self::Specific(s) => Some(s.len()),
        }
    }

    #[allow(dead_code)]
    fn union(a: Self, b: Self) -> Self {
        match (a, b) {
            (Self::NotSet, x) => x,
            (x, Self::NotSet) => x,
            (Self::All, _) => Self::All,
            (_, Self::All) => Self::All,
            (Self::Specific(s1), Self::Specific(s2)) => {
                Self::Specific(s1.union(&s2).cloned().collect())
            }
        }
    }

    fn intersection(a: Self, b: Self) -> Self {
        match (a, b) {
            (Self::NotSet, x) => x,
            (x, Self::NotSet) => x,
            (Self::All, x) => x,
            (x, Self::All) => x,
            (Self::Specific(s1), Self::Specific(s2)) => {
                Self::Specific(s1.intersection(&s2).cloned().collect())
            }
        }
    }

    #[allow(dead_code)]
    fn is_empty(&self) -> bool {
        match self {
            Self::NotSet => true,
            Self::All => false,
            Self::Specific(s) => s.is_empty(),
        }
    }
}

impl From<Subset> for HashSet<UtxoRef> {
    fn from(value: Subset) -> Self {
        match value {
            Subset::Specific(s) => s,
            Subset::NotSet => HashSet::new(),
            Subset::All => HashSet::new(),
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
    pub union: Subset,
    pub intersection: Subset,
    pub by_address_count: Option<usize>,
    pub by_asset_class_count: Option<usize>,
    pub by_ref_count: Option<usize>,
}

impl SearchSpace {
    fn new() -> Self {
        Self {
            union: Subset::NotSet,
            intersection: Subset::NotSet,
            by_address_count: None,
            by_asset_class_count: None,
            by_ref_count: None,
        }
    }

    fn is_constrained(&self) -> bool {
        match &self.intersection {
            Subset::NotSet => false,
            Subset::All => false,
            Subset::Specific(_) => true,
        }
    }

    fn include_subset(&mut self, subset: Subset) {
        self.union = Subset::union(self.union.clone(), subset.clone());
        self.intersection = Subset::intersection(self.intersection.clone(), subset);
    }

    fn include_matches(&mut self, utxos: HashSet<UtxoRef>) {
        let matches = Subset::Specific(utxos);
        self.include_subset(matches);
    }

    fn include_address_matches(&mut self, subset: Subset) {
        *self.by_address_count.get_or_insert(0) += subset.count().unwrap_or(0);
        self.include_subset(subset);
    }

    fn include_asset_class_matches(&mut self, subset: Subset) {
        *self.by_asset_class_count.get_or_insert(0) += subset.count().unwrap_or(0);
        self.include_subset(subset);
    }

    fn add_ref_matches(&mut self, utxos: HashSet<UtxoRef>) {
        *self.by_ref_count.get_or_insert(0) += utxos.len();
        self.include_matches(utxos);
    }

    pub fn take(&self, take: Option<usize>) -> HashSet<UtxoRef> {
        let Some(take) = take else {
            // if there's no limit, return everything we have
            return self.union.clone().into();
        };

        // if we have a specific limit, we need to pick the best options. The
        // intersection are the best matches since they are the most specific, so we
        // take from them first. If we don't have enough, we take the remaining from the
        // union.

        let best: HashSet<_> = self.intersection.clone().into();

        if best.len() < take {
            let others: HashSet<_> = self.union.clone().into();
            let diff: HashSet<_> = others.difference(&best).cloned().collect();
            let remaining: HashSet<_> = diff.into_iter().take(take - best.len()).collect();
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
        return Ok(parent);
    }

    let AssetClass::Defined(policy, name) = class else {
        return Ok(parent);
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

    search_space.include_address_matches(parent_subset.clone());

    if let Some(assets) = &criteria.min_amount {
        for (class, amount) in assets.iter() {
            if *amount > 0 {
                let subset = narrow_by_asset_class(store, parent_subset.clone(), class).await?;
                search_space.include_asset_class_matches(subset);
            }
        }
    }

    if !criteria.refs.is_empty() {
        search_space.add_ref_matches(criteria.refs.clone());
    }

    if !search_space.is_constrained() {
        dbg!(&search_space);
        return Err(Error::InputQueryTooBroad);
    }

    Ok(search_space)
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use tx3_tir::model::assets::CanonicalAssets;

    use super::*;

    use crate::mock;

    fn assets_for(asset: mock::KnownAsset, amount: i128) -> CanonicalAssets {
        CanonicalAssets::from_asset(
            Some(asset.policy().as_ref()),
            Some(asset.name().as_ref()),
            amount,
        )
    }

    fn cq(
        address: Option<&mock::KnownAddress>,
        min_assets: Option<CanonicalAssets>,
        refs: HashSet<UtxoRef>,
    ) -> CanonicalQuery {
        CanonicalQuery {
            address: address.map(|a| a.to_bytes()),
            min_amount: min_assets,
            refs,
            support_many: true,
            collateral: false,
        }
    }

    fn prepare_store() -> mock::MockStore {
        mock::seed_random_memory_store(
            |_: &mock::FuzzTxoRef, x: &mock::KnownAddress, seq: u64| {
                if seq % 2 == 0 {
                    mock::utxo_with_random_asset(x, mock::KnownAsset::Hosky, 500..1000)
                } else {
                    mock::utxo_with_random_amount(x, 4_000_000..5_000_000)
                }
            },
            2..3,
        )
    }

    async fn assert_space_matches<T: UtxoStore>(
        store: &T,
        criteria: CanonicalQuery,
        expected: HashSet<UtxoRef>,
    ) {
        let space = narrow_search_space(store, &criteria).await.unwrap();
        let got = space.take(Some(expected.len()));
        assert_eq!(got, expected);
    }

    #[pollster::test]
    async fn test_address_only() {
        let store = prepare_store();

        let addr = mock::KnownAddress::Alice;
        let expected = store.by_known_address(&addr).await;

        let criteria = cq(Some(&addr), None, HashSet::new());
        assert_space_matches(&store, criteria, expected).await;
    }

    #[pollster::test]
    async fn test_asset_only() {
        let store = prepare_store();

        let asset = mock::KnownAsset::Hosky;
        let expected = store.by_known_asset(&asset).await;

        let min_assets = assets_for(asset, 1);
        let criteria = cq(None, Some(min_assets), HashSet::new());
        assert_space_matches(&store, criteria, expected).await;
    }

    #[pollster::test]
    async fn test_refs_only() {
        let store = prepare_store();

        let alice = mock::KnownAddress::Alice;
        let bob = mock::KnownAddress::Bob;

        let alice_refs = store.by_known_address(&alice).await;
        let bob_refs = store.by_known_address(&bob).await;

        let pick_one = |set: &HashSet<UtxoRef>| set.iter().next().unwrap().clone();
        let refs: HashSet<UtxoRef> =
            HashSet::from_iter(vec![pick_one(&alice_refs), pick_one(&bob_refs)]);

        let criteria = cq(None, None, refs.clone());
        assert_space_matches(&store, criteria, refs).await;
    }

    #[pollster::test]
    async fn test_address_and_asset_intersection() {
        let store = prepare_store();

        let addr = mock::KnownAddress::Alice;
        let asset = mock::KnownAsset::Hosky;

        let by_addr = store.by_known_address(&addr).await;
        let by_asset = store.by_known_asset(&asset).await;

        let expected_best: HashSet<_> = by_addr.intersection(&by_asset).cloned().collect();

        let min_assets = assets_for(asset, 1);
        let criteria = cq(Some(&addr), Some(min_assets), HashSet::new());
        assert_space_matches(&store, criteria, expected_best).await;
    }

    #[pollster::test]
    async fn test_address_and_refs_intersection() {
        let store = prepare_store();

        let alice = mock::KnownAddress::Alice;
        let bob = mock::KnownAddress::Bob;

        let alice_refs = store.by_known_address(&alice).await;
        let bob_refs = store.by_known_address(&bob).await;

        let pick_one = |set: &HashSet<UtxoRef>| set.iter().next().unwrap().clone();
        let refs: HashSet<UtxoRef> =
            HashSet::from_iter(vec![pick_one(&alice_refs), pick_one(&bob_refs)]);

        let expected_best: HashSet<_> = alice_refs.intersection(&refs).cloned().collect();

        let criteria = cq(Some(&alice), None, refs);
        assert_space_matches(&store, criteria, expected_best).await;
    }

    #[pollster::test]
    async fn test_asset_and_refs_intersection() {
        let store = prepare_store();

        let asset = mock::KnownAsset::Hosky;

        let by_asset = store.by_known_asset(&asset).await;

        // pick one ref that surely has the asset, and another arbitrary ref from
        // someone else
        let alice = mock::KnownAddress::Alice;
        let bob = mock::KnownAddress::Bob;

        let alice_any = store.by_known_address(&alice).await;
        let bob_any = store.by_known_address(&bob).await;

        let pick_one = |set: &HashSet<UtxoRef>| set.iter().next().unwrap().clone();
        let one_with_asset = pick_one(&by_asset);
        let other_ref = pick_one(&bob_any.union(&alice_any).cloned().collect());

        let refs: HashSet<UtxoRef> = HashSet::from_iter(vec![one_with_asset.clone(), other_ref]);
        let expected_best: HashSet<_> = by_asset.intersection(&refs).cloned().collect();

        let min_assets = assets_for(asset, 1);
        let criteria = cq(None, Some(min_assets), refs);
        assert_space_matches(&store, criteria, expected_best).await;
    }

    #[pollster::test]
    async fn test_address_asset_and_refs_intersection() {
        let store = prepare_store();

        let addr = mock::KnownAddress::Alice;
        let asset = mock::KnownAsset::Hosky;

        let by_addr = store.by_known_address(&addr).await;
        let by_asset = store.by_known_asset(&asset).await;

        let both: HashSet<_> = by_addr.intersection(&by_asset).cloned().collect();
        assert!(!both.is_empty());

        let one_ref = both.iter().next().unwrap().clone();
        let mut refs = HashSet::new();
        refs.insert(one_ref.clone());

        // include a distractor ref that does not satisfy all dims
        let bob = mock::KnownAddress::Bob;
        let bob_refs = store
            .narrow_refs(UtxoPattern::by_address(&bob.to_bytes()))
            .await
            .unwrap();
        let distractor = bob_refs.iter().next().unwrap().clone();
        refs.insert(distractor);

        let expected_best: HashSet<_> = both.intersection(&refs).cloned().collect();

        let min_assets = assets_for(asset, 1);
        let criteria = cq(Some(&addr), Some(min_assets), refs);
        assert_space_matches(&store, criteria, expected_best).await;
    }
}
