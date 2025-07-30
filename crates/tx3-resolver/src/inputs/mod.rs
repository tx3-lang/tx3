//! Tx input selection algorithms

use std::collections::{BTreeMap, HashSet};
use tx3_lang::{
    applying,
    backend::{self, UtxoStore},
    ir, CanonicalAssets, Utxo, UtxoRef, UtxoSet,
};

use crate::inputs::searching::SearchSpace;

mod searching;

macro_rules! data_or_bail {
    ($expr:expr, bytes) => {
        $expr
            .as_bytes()
            .ok_or(Error::ExpectedData("bytes".to_string(), $expr.clone()))
    };

    ($expr:expr, number) => {
        $expr
            .as_number()
            .ok_or(Error::ExpectedData("number".to_string(), $expr.clone()))?
    };

    ($expr:expr, assets) => {
        $expr
            .as_assets()
            .ok_or(Error::ExpectedData("assets".to_string(), $expr.clone()))
    };

    ($expr:expr, utxo_refs) => {
        $expr
            .as_utxo_refs()
            .ok_or(Error::ExpectedData("utxo refs".to_string(), $expr.clone()))
    };
}

pub struct Diagnostic {
    pub query: ir::InputQuery,
    pub utxos: UtxoSet,
    pub selected: UtxoSet,
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("expected {0}, got {1:?}")]
    ExpectedData(String, ir::Expression),

    #[error("input query too broad")]
    InputQueryTooBroad,

    #[error("input not resolved: {0} {1} {2}")]
    InputNotResolved(String, CanonicalQuery, SearchSpace),

    #[error("store error: {0}")]
    StoreError(#[from] backend::Error),

    #[error("apply error: {0}")]
    ApplyError(#[from] applying::Error),
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

    pub async fn select(
        &mut self,
        search_space: &SearchSpace,
        criteria: &CanonicalQuery,
    ) -> Result<UtxoSet, Error> {
        let refs = search_space
            .matched
            .clone()
            .into_iter()
            .filter(|x| !self.ignore.contains(x))
            .collect();

        let utxos = self.store.fetch_utxos(refs).await?;

        let target = criteria
            .min_amount
            .clone()
            .unwrap_or(CanonicalAssets::empty());

        let matched = if criteria.collateral {
            CollateralMatch::pick(utxos, &target)
        } else if criteria.support_many {
            NaiveAccumulator::pick(utxos, &target)
        } else {
            FirstFullMatch::pick(utxos, &target)
        };

        self.ignore.extend(matched.iter().map(|x| x.r#ref.clone()));

        Ok(matched)
    }
}

#[derive(Debug, Clone)]
pub struct CanonicalQuery {
    pub address: Option<Vec<u8>>,
    pub min_amount: Option<CanonicalAssets>,
    pub refs: HashSet<UtxoRef>,
    pub support_many: bool,
    pub collateral: bool,
}

impl std::fmt::Display for CanonicalQuery {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CanonicalQuery {{")?;

        if let Some(address) = &self.address {
            write!(f, "address: {}", hex::encode(address))?;
        }

        if let Some(min_amount) = &self.min_amount {
            write!(f, "min_amount: {}", min_amount)?;
        }

        for (i, ref_) in self.refs.iter().enumerate() {
            write!(f, "ref[{}]:{}#{}", i, hex::encode(&ref_.txid), ref_.index)?;
        }

        write!(f, "support_many: {:?}", self.support_many)?;
        write!(f, "for_collateral: {:?}", self.collateral)?;
        write!(f, "}}")
    }
}

impl TryFrom<ir::InputQuery> for CanonicalQuery {
    type Error = Error;

    fn try_from(query: ir::InputQuery) -> Result<Self, Self::Error> {
        let address = query
            .address
            .as_option()
            .map(|x| data_or_bail!(x, bytes))
            .transpose()?
            .map(|x| Vec::from(x));

        let min_amount = query
            .min_amount
            .as_option()
            .map(|x| data_or_bail!(x, assets))
            .transpose()?
            .map(|x| CanonicalAssets::from(Vec::from(x)));

        let refs = query
            .r#ref
            .as_option()
            .map(|x| data_or_bail!(x, utxo_refs))
            .transpose()?
            .map(|x| HashSet::from_iter(x.iter().cloned()))
            .unwrap_or_default();

        Ok(Self {
            address,
            min_amount,
            refs,
            support_many: query.many,
            collateral: query.collateral,
        })
    }
}

pub async fn resolve<T: UtxoStore>(tx: ir::Tx, utxos: &T) -> Result<ir::Tx, Error> {
    let mut all_inputs = BTreeMap::new();

    let mut selector = InputSelector::new(utxos);

    for (name, query) in applying::find_queries(&tx) {
        let query = CanonicalQuery::try_from(query)?;

        let space = searching::narrow_search_space(utxos, &query, MAX_SEARCH_SPACE_SIZE).await?;

        let utxos = selector.select(&space, &query).await?;

        if utxos.is_empty() {
            return Err(Error::InputNotResolved(name.to_string(), query, space));
        }

        all_inputs.insert(name, utxos);
    }

    let out = applying::apply_inputs(tx, &all_inputs)?;

    Ok(out)
}

#[cfg(test)]
mod tests {
    use crate::mock;

    use super::*;

    pub fn new_input_query(
        address: &mock::KnownAddress,
        naked_amount: Option<u64>,
        other_assets: Vec<(mock::KnownAsset, u64)>,
        many: bool,
        collateral: bool,
    ) -> CanonicalQuery {
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
        .try_into()
        .unwrap()
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

            let space = searching::narrow_search_space(&store, &criteria, MAX_SEARCH_SPACE_SIZE)
                .await
                .unwrap();

            let utxos = selector.select(&space, &criteria).await.unwrap();

            assert_eq!(utxos.len(), 1);

            for utxo in utxos {
                assert_eq!(utxo.address, subject.to_bytes());
            }
        }
    }

    #[pollster::test]
    async fn test_input_query_too_broad() {
        let store = mock::seed_random_memory_store(
            |_: &mock::FuzzTxoRef, x: &mock::KnownAddress, _: u64| {
                mock::utxo_with_random_amount(x, 4_000_000..5_000_000)
            },
            2..4,
        );

        let empty_criteria = ir::InputQuery {
            address: ir::Expression::None,
            min_amount: ir::Expression::None,
            r#ref: ir::Expression::None,
            many: false,
            collateral: false,
        }
        .try_into()
        .unwrap();

        let space = searching::narrow_search_space(&store, &empty_criteria, MAX_SEARCH_SPACE_SIZE)
            .await
            .unwrap_err();

        assert!(matches!(space, Error::InputQueryTooBroad));
    }

    #[pollster::test]
    async fn test_select_anything() {
        let store = mock::seed_random_memory_store(
            |_: &mock::FuzzTxoRef, x: &mock::KnownAddress, seq: u64| {
                if seq % 2 == 0 {
                    mock::utxo_with_random_amount(x, 4_000_000..5_000_000)
                } else {
                    mock::utxo_with_random_asset(x, mock::KnownAsset::Hosky, 500..1000)
                }
            },
            2..3, // exclusive range, this means always two utxos per address
        );

        let mut selector = InputSelector::new(&store);

        let criteria = new_input_query(&mock::KnownAddress::Alice, None, vec![], true, false);

        let space = searching::narrow_search_space(&store, &criteria, MAX_SEARCH_SPACE_SIZE)
            .await
            .unwrap();

        let utxos = selector.select(&space, &criteria).await.unwrap();
        assert_eq!(utxos.len(), 1);
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

        let space = searching::narrow_search_space(&store, &criteria, MAX_SEARCH_SPACE_SIZE)
            .await
            .unwrap();

        let utxos = selector.select(&space, &criteria).await.unwrap();
        assert!(utxos.is_empty());

        let criteria = new_input_query(
            &mock::KnownAddress::Alice,
            Some(4_000_000),
            vec![],
            false,
            false,
        );

        let utxos = selector.select(&space, &criteria).await.unwrap();

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

            let space = searching::narrow_search_space(&store, &criteria, MAX_SEARCH_SPACE_SIZE)
                .await
                .unwrap();

            let mut selector = InputSelector::new(&store);
            let utxos = selector.select(&space, &criteria).await.unwrap();
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

            let space = searching::narrow_search_space(&store, &criteria, MAX_SEARCH_SPACE_SIZE)
                .await
                .unwrap();

            let mut selector = InputSelector::new(&store);
            let utxos = selector.select(&space, &criteria).await.unwrap();
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

            let space = searching::narrow_search_space(&store, &criteria, MAX_SEARCH_SPACE_SIZE)
                .await
                .unwrap();

            let mut selector = InputSelector::new(&store);
            let utxos = selector.select(&space, &criteria).await.unwrap();
            assert!(utxos.is_empty());

            // test negative case where we ask for a different asset

            let criteria = new_input_query(
                &address,
                None,
                vec![(mock::KnownAsset::Snek, 500)],
                false,
                false,
            );

            let space = searching::narrow_search_space(&store, &criteria, MAX_SEARCH_SPACE_SIZE)
                .await
                .unwrap();

            let mut selector = InputSelector::new(&store);
            let utxos = selector.select(&space, &criteria).await.unwrap();
            assert!(utxos.is_empty());

            // test positive case where we ask for the present asset and amount within range

            let criteria = new_input_query(
                &address,
                None,
                vec![(mock::KnownAsset::Hosky, 500)],
                false,
                false,
            );

            let space = searching::narrow_search_space(&store, &criteria, MAX_SEARCH_SPACE_SIZE)
                .await
                .unwrap();

            let mut selector = InputSelector::new(&store);
            let utxos = selector.select(&space, &criteria).await.unwrap();
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

            let space = searching::narrow_search_space(&store, &criteria, MAX_SEARCH_SPACE_SIZE)
                .await
                .unwrap();

            let utxos = selector.select(&space, &criteria).await.unwrap();

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

            let space = searching::narrow_search_space(&store, &query1, MAX_SEARCH_SPACE_SIZE)
                .await
                .unwrap();

            let utxos1 = selector.select(&space, &query1).await.unwrap();
            assert_eq!(utxos1.len(), 1);

            // we ask for a large amount utxo again knowing that we already selected one of
            // those
            let query2 = new_input_query(&address, Some(1_000), vec![], true, false);

            let space = searching::narrow_search_space(&store, &query2, MAX_SEARCH_SPACE_SIZE)
                .await
                .unwrap();

            let utxos2 = selector.select(&space, &query2).await.unwrap();
            assert_eq!(utxos2.len(), 0);

            // we ask for a small amount utxo knowing that we have one of those
            let query3 = new_input_query(&address, Some(100), vec![], true, false);

            let space = searching::narrow_search_space(&store, &query3, MAX_SEARCH_SPACE_SIZE)
                .await
                .unwrap();

            let utxos3 = selector.select(&space, &query3).await.unwrap();
            assert_eq!(utxos3.len(), 1);

            // we ask for a small amount utxo again knowing that we already selected one of
            // those
            let query4 = new_input_query(&address, Some(100), vec![], true, false);

            let space = searching::narrow_search_space(&store, &query4, MAX_SEARCH_SPACE_SIZE)
                .await
                .unwrap();

            let utxos4 = selector.select(&space, &query4).await.unwrap();
            assert_eq!(utxos4.len(), 0);

            // we ensure that selected utxos are not the same
            utxos1.is_disjoint(&utxos3);
        }
    }
}
