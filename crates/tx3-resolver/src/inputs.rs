//! Tx input selection algorithms

use std::collections::{BTreeMap, HashSet};
use tx3_lang::{
    applying,
    backend::{UtxoPattern, UtxoStore},
    ir, CanonicalAssets, Utxo, UtxoRef, UtxoSet,
};

use crate::Error;

macro_rules! data_or_bail {
    ($expr:expr, bytes) => {
        $expr
            .as_bytes()
            .ok_or(Error::ExpectedData("bytes".to_string(), $expr.clone()))?
    };

    ($expr:expr, number) => {
        $expr
            .as_number()
            .ok_or(Error::ExpectedData("number".to_string(), $expr.clone()))?
    };

    ($expr:expr, assets) => {
        $expr
            .as_assets()
            .ok_or(Error::ExpectedData("assets".to_string(), $expr.clone()))?
    };

    ($expr:expr, utxo_refs) => {
        $expr
            .as_utxo_refs()
            .ok_or(Error::ExpectedData("utxo refs".to_string(), $expr.clone()))?
    };
}

enum Subset {
    All,
    Specific(HashSet<UtxoRef>),
}

impl Subset {
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

pub trait CoinSelection {
    fn pick(search_space: UtxoSet, target: &CanonicalAssets) -> HashSet<Utxo>;
}

struct FirstFullMatch;

impl CoinSelection for FirstFullMatch {
    fn pick(search_space: UtxoSet, target: &CanonicalAssets) -> HashSet<Utxo> {
        for utxo in search_space.iter() {
            if utxo.assets.contains_total(target) {
                return HashSet::from_iter(vec![utxo.clone()]);
            }
        }

        HashSet::new()
    }
}

fn find_first_excess_utxo(utxos: &HashSet<Utxo>, target: &CanonicalAssets) -> Option<Utxo> {
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

struct NaiveAccumulator;

impl CoinSelection for NaiveAccumulator {
    fn pick(search_space: UtxoSet, target: &CanonicalAssets) -> HashSet<Utxo> {
        let mut matched = HashSet::new();
        let mut pending = target.clone();

        for utxo in search_space.iter() {
            if utxo.assets.contains_some(target) {
                matched.insert(utxo.clone());
                pending = pending - utxo.assets.clone();
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

        while let Some(utxo) = find_first_excess_utxo(&matched, target) {
            matched.remove(&utxo);
        }

        matched
    }
}

struct CollateralMatch;

impl CoinSelection for CollateralMatch {
    fn pick(search_space: UtxoSet, target: &CanonicalAssets) -> HashSet<Utxo> {
        for utxo in search_space.iter() {
            let only_naked = utxo.assets.keys().all(|x| x.is_naked());
            let has_enough = utxo.assets.contains_total(target);

            if only_naked && has_enough {
                return HashSet::from_iter(vec![utxo.clone()]);
            }
        }

        HashSet::new()
    }
}

const MAX_SEARCH_SPACE_SIZE: usize = 50;

struct InputSelector<'a, S: UtxoStore> {
    store: &'a S,
    ignore: HashSet<UtxoRef>,
}

impl<'a, S: UtxoStore> InputSelector<'a, S> {
    pub fn new(store: &'a S) -> Self {
        Self {
            store,
            ignore: HashSet::new(),
        }
    }

    async fn narrow_by_address(&self, expr: &ir::Expression) -> Result<Subset, Error> {
        let address = data_or_bail!(expr, bytes);

        let utxos = self
            .store
            .narrow_refs(UtxoPattern::by_address(address))
            .await?;

        Ok(Subset::Specific(utxos.into_iter().collect()))
    }

    async fn narrow_by_asset_presence(&self, expr: &ir::AssetExpr) -> Result<Subset, Error> {
        let amount = data_or_bail!(&expr.amount, number);

        // skip filtering if required amount is 0 since it's not adding any constraints
        if amount == 0 {
            return Ok(Subset::All);
        }

        // skip filtering lovelace since it's not an custom asset
        if expr.policy.is_none() {
            return Ok(Subset::All);
        }

        let policy_bytes = data_or_bail!(&expr.policy, bytes);
        let name_bytes = data_or_bail!(&expr.asset_name, bytes);

        let utxos = self
            .store
            .narrow_refs(UtxoPattern::by_asset(policy_bytes, name_bytes))
            .await?;

        Ok(Subset::Specific(utxos.into_iter().collect()))
    }

    async fn narrow_by_multi_asset_presence(&self, expr: &ir::Expression) -> Result<Subset, Error> {
        let assets = data_or_bail!(expr, assets);

        let mut matches = Subset::All;

        for asset in assets {
            let next = self.narrow_by_asset_presence(asset).await?;
            matches = Subset::intersection(matches, next);
        }

        Ok(matches)
    }

    fn narrow_by_ref(&self, expr: &ir::Expression) -> Result<Subset, Error> {
        let refs = data_or_bail!(expr, utxo_refs);

        let refs = HashSet::from_iter(refs.iter().cloned());

        Ok(Subset::Specific(refs))
    }

    async fn narrow_search_space(&self, criteria: &ir::InputQuery) -> Result<Subset, Error> {
        let matching_address = if let Some(address) = &criteria.address.as_option() {
            self.narrow_by_address(address).await?
        } else {
            Subset::All
        };

        if matching_address.is_empty() {
            // TODO: track this as part of a detailed diagnostic
        }

        let matching_assets = if let Some(min_amount) = &criteria.min_amount.as_option() {
            self.narrow_by_multi_asset_presence(min_amount).await?
        } else {
            Subset::All
        };

        if matching_assets.is_empty() {
            // TODO: track this as part of a detailed diagnostic
        }

        let matching_refs = if let Some(refs) = &criteria.r#ref.as_option() {
            self.narrow_by_ref(refs)?
        } else {
            Subset::All
        };

        if matching_refs.is_empty() {
            // TODO: track this as part of a detailed diagnostic
        }

        Ok(Subset::intersection_of_all([
            matching_address,
            matching_assets,
            matching_refs,
        ]))
    }

    pub async fn select(&mut self, criteria: &ir::InputQuery) -> Result<UtxoSet, Error> {
        let search_space = self.narrow_search_space(criteria).await?;

        let refs = match search_space {
            Subset::Specific(refs) if refs.len() <= MAX_SEARCH_SPACE_SIZE => refs,
            Subset::Specific(_) => return Err(Error::InputQueryTooBroad),
            Subset::All => return Err(Error::InputQueryTooBroad),
        };

        let refs = refs
            .into_iter()
            .filter(|x| !self.ignore.contains(x))
            .collect();

        let utxos = self.store.fetch_utxos(refs).await?;

        let target = data_or_bail!(&criteria.min_amount, assets);
        let target = CanonicalAssets::from(Vec::from(target));

        let matched = if criteria.collateral {
            CollateralMatch::pick(utxos, &target)
        } else if criteria.many {
            NaiveAccumulator::pick(utxos, &target)
        } else {
            FirstFullMatch::pick(utxos, &target)
        };

        self.ignore.extend(matched.iter().map(|x| x.r#ref.clone()));

        Ok(matched)
    }
}

pub async fn resolve<T: UtxoStore>(tx: ir::Tx, utxos: &T) -> Result<ir::Tx, Error> {
    let mut all_inputs = BTreeMap::new();

    let mut selector = InputSelector::new(utxos);

    for (name, query) in applying::find_queries(&tx) {
        let utxos = selector.select(&query).await?;

        if utxos.is_empty() {
            return Err(Error::InputNotResolved(name, query));
        }

        all_inputs.insert(name, utxos);
    }

    let out = applying::apply_inputs(tx, &all_inputs)?;

    Ok(out)
}

#[cfg(test)]
mod tests {
    use chainfuzz::utxos::utxo_with_random_amount;

    use crate::mock;

    use super::*;

    pub fn new_input_query(
        address: &mock::KnownAddress,
        naked_amount: Option<u64>,
        other_assets: Vec<(mock::KnownAsset, u64)>,
        many: bool,
        collateral: bool,
    ) -> ir::InputQuery {
        let naked_asset = naked_amount.map(|x| ir::AssetExpr {
            policy: ir::Expression::None,
            asset_name: ir::Expression::None,
            amount: ir::Expression::Number(x as i128),
        });

        let other_assets: Vec<ir::AssetExpr> = other_assets
            .into_iter()
            .map(|(asset, amount)| ir::AssetExpr {
                policy: ir::Expression::Bytes(asset.policy().as_slice().to_vec()),
                asset_name: ir::Expression::Bytes(asset.name().to_vec()),
                amount: ir::Expression::Number(amount as i128),
            })
            .collect();

        let all_assets = naked_asset.into_iter().chain(other_assets).collect();

        ir::InputQuery {
            address: ir::Expression::Address(address.to_bytes()),
            min_amount: ir::Expression::Assets(all_assets),
            r#ref: ir::Expression::None,
            many,
            collateral,
        }
    }

    #[pollster::test]
    async fn test_select_by_address() {
        let store = mock::seed_random_memory_store(
            |_: &mock::FuzzTxoRef, x: &mock::KnownAddress, _: u64| {
                mock::utxo_with_random_amount(x, 4_000_000..5_000_000)
            },
            2..4,
        );

        let mut selector = InputSelector::new(&store);

        for subject in mock::KnownAddress::everyone() {
            let criteria = new_input_query(&subject, None, vec![], false, false);

            let utxos = selector.select(&criteria).await.unwrap();

            assert_eq!(utxos.len(), 1);

            for utxo in utxos {
                assert_eq!(utxo.address, subject.to_bytes());
            }
        }
    }

    #[pollster::test]
    async fn test_select_by_naked_amount() {
        let store = mock::seed_random_memory_store(
            |_: &mock::FuzzTxoRef, x: &mock::KnownAddress, _: u64| {
                mock::utxo_with_random_amount(x, 4_000_000..5_000_000)
            },
            2..4,
        );

        let mut selector = InputSelector::new(&store);

        let criteria = new_input_query(
            &mock::KnownAddress::Alice,
            Some(6_000_000),
            vec![],
            false,
            false,
        );

        let utxos = selector.select(&criteria).await.unwrap();
        assert!(utxos.is_empty());

        let criteria = new_input_query(
            &mock::KnownAddress::Alice,
            Some(4_000_000),
            vec![],
            false,
            false,
        );

        let utxos = selector.select(&criteria).await.unwrap();

        let match_count = dbg!(utxos.len());
        assert_eq!(match_count, 1);
    }

    #[pollster::test]
    async fn test_select_by_asset_amount() {
        let store = mock::seed_random_memory_store(
            |_: &mock::FuzzTxoRef, x: &mock::KnownAddress, _: u64| {
                mock::utxo_with_random_asset(x, mock::KnownAsset::Hosky, 500..1000)
            },
            2..4,
        );

        for address in mock::KnownAddress::everyone() {
            // test negative case where we ask for a single utxo with more than available
            // amount

            let criteria = new_input_query(
                &address,
                None,
                vec![(mock::KnownAsset::Hosky, 1001)],
                false,
                false,
            );

            let mut selector = InputSelector::new(&store);
            let utxos = selector.select(&criteria).await.unwrap();
            assert!(utxos.is_empty());

            // test positive case where we ask for any number of utxo adding to the target
            // amount

            let criteria = new_input_query(
                &address,
                None,
                vec![(mock::KnownAsset::Hosky, 1001)],
                true,
                false,
            );

            let mut selector = InputSelector::new(&store);
            let utxos = selector.select(&criteria).await.unwrap();
            assert!(utxos.len() > 1);

            // test negative case where we ask for any number of utxo adding to the target
            // amount that is not possible

            let criteria = new_input_query(
                &address,
                None,
                vec![(mock::KnownAsset::Hosky, 4001)],
                true,
                false,
            );

            let mut selector = InputSelector::new(&store);
            let utxos = selector.select(&criteria).await.unwrap();
            assert!(utxos.is_empty());

            // test negative case where we ask for a different asset

            let criteria = new_input_query(
                &address,
                None,
                vec![(mock::KnownAsset::Snek, 500)],
                false,
                false,
            );

            let mut selector = InputSelector::new(&store);
            let utxos = selector.select(&criteria).await.unwrap();
            assert!(utxos.is_empty());

            // test positive case where we ask for the present asset and amount within range

            let criteria = new_input_query(
                &address,
                None,
                vec![(mock::KnownAsset::Hosky, 500)],
                false,
                false,
            );

            let mut selector = InputSelector::new(&store);
            let utxos = selector.select(&criteria).await.unwrap();
            assert_eq!(utxos.len(), 1);
        }
    }

    #[pollster::test]
    async fn test_select_by_collateral() {
        let store = mock::seed_random_memory_store(
            |_: &mock::FuzzTxoRef, x: &mock::KnownAddress, sequence: u64| {
                if sequence % 2 == 0 {
                    mock::utxo_with_random_amount(x, 4_000_000..5_000_000)
                } else {
                    mock::utxo_with_random_asset(x, mock::KnownAsset::Hosky, 500..1000)
                }
            },
            2..4,
        );

        let mut selector = InputSelector::new(&store);

        for address in mock::KnownAddress::everyone() {
            let criteria = new_input_query(&address, Some(1_000_000), vec![], false, true);

            let utxos = selector.select(&criteria).await.unwrap();

            assert_eq!(utxos.len(), 1);
            let utxo = utxos.iter().next().unwrap();
            assert_eq!(utxo.assets.keys().len(), 1);
            assert_eq!(utxo.assets.keys().next().unwrap().is_naked(), true);
        }
    }

    #[pollster::test]
    async fn test_resolve_ignores_selected_utxos() {
        let store = mock::seed_random_memory_store(
            |_: &mock::FuzzTxoRef, x: &mock::KnownAddress, seq: u64| {
                if seq % 2 == 0 {
                    mock::utxo_with_random_amount(x, 1_000..1_500)
                } else {
                    mock::utxo_with_random_amount(x, 100..150)
                }
            },
            2..3, // exclusive range, this means only two utxos per address
        );

        for address in mock::KnownAddress::everyone() {
            let mut selector = InputSelector::new(&store);

            // we ask for a large amount utxo knowing that we have one of those
            let query1 = new_input_query(&address, Some(1_000), vec![], true, false);
            let utxos1 = selector.select(&query1).await.unwrap();
            assert_eq!(utxos1.len(), 1);

            // we ask for a large amount utxo again knowing that we already selected one of
            // those
            let query2 = new_input_query(&address, Some(1_000), vec![], true, false);
            let utxos2 = selector.select(&query2).await.unwrap();
            assert_eq!(utxos2.len(), 0);

            // we ask for a small amount utxo knowing that we have one of those
            let query3 = new_input_query(&address, Some(100), vec![], true, false);
            let utxos3 = selector.select(&query3).await.unwrap();
            assert_eq!(utxos3.len(), 1);

            // we ask for a small amount utxo again knowing that we already selected one of
            // those
            let query4 = new_input_query(&address, Some(100), vec![], true, false);
            let utxos4 = selector.select(&query4).await.unwrap();
            assert_eq!(utxos4.len(), 0);

            // we ensure that selected utxos are not the same
            utxos1.is_disjoint(&utxos3);
        }
    }
}
