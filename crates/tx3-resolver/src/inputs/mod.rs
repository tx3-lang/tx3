//! Tx input selection algorithms

use std::collections::{BTreeMap, HashSet};
use std::hash::{Hash, Hasher};
use tx3_lang::{
    applying,
    backend::{self, UtxoStore},
    ir, CanonicalAssets, Utxo, UtxoRef, UtxoSet,
};

pub use crate::inputs::searching::SearchSpace;

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

    #[error("input not resolved: {0}")]
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

        while let Some(utxo) = find_first_excess_utxo(&matched, target) {
            matched.remove(&utxo);
        }

        matched
    }
}

struct VectorAccumulator;

impl CoinSelection for VectorAccumulator {
    fn pick(search_space: UtxoSet, target: &CanonicalAssets) -> HashSet<Utxo> {
        let mut candidates: Vec<Utxo> = search_space.iter().cloned().collect();
        let mut matched = HashSet::new();
        let mut pending = target.clone();
        let vec_target = to_vector(&pending);

        candidates.sort_by(|a, b| {
            // 1. Prefer candidates that cover more of the required asset classes.
            // We count how many asset classes from the target are missing in the candidate.
            // Lower is better.
            let missing_a = count_missing(&a.assets, &pending);
            let missing_b = count_missing(&b.assets, &pending);
            if missing_a != missing_b {
                return missing_a.cmp(&missing_b);
            }

            // 2. Prefer candidates that have fewer unrequested asset classes.
            // We count how many asset classes in the candidate are not in the target.
            // Lower is better (cleaner match).
            let unreq_a = count_unrequested(&a.assets, &pending);
            let unreq_b = count_unrequested(&b.assets, &pending);
            if unreq_a != unreq_b {
                return unreq_a.cmp(&unreq_b);
            }

            // 3. Prefer candidates that are closer to the target amount vector.
            // We calculate a vector distance (e.g. Euclidean) to find the best fit in terms of quantities.
            let vec_a = to_vector(&a.assets);
            let vec_b = to_vector(&b.assets);

            let dist_a = vector_distance(&vec_a, &vec_target);
            let dist_b = vector_distance(&vec_b, &vec_target);
            dist_a.cmp(&dist_b)
        });

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

fn to_vector(assets: &CanonicalAssets) -> [i128; 8] {
    let mut vec = [0; 8];
    for (class, amount) in assets.iter() {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        class.hash(&mut hasher);
        let idx = (hasher.finish() % 8) as usize;
        vec[idx] += amount;
    }
    vec
}

fn vector_distance(a: &[i128; 8], b: &[i128; 8]) -> i128 {
    a.iter().zip(b.iter()).map(|(x, y)| (x - y).pow(2)).sum()
}

fn get_amount(assets: &CanonicalAssets, class: &tx3_lang::AssetClass) -> i128 {
    for (c, a) in assets.iter() {
        if c == class {
            return *a;
        }
    }
    0
}

fn count_unrequested(assets: &CanonicalAssets, target: &CanonicalAssets) -> usize {
    let mut count = 0;
    for (class, _) in assets.iter() {
        if get_amount(target, class) == 0 {
            count += 1;
        }
    }
    count
}

fn count_missing(assets: &CanonicalAssets, target: &CanonicalAssets) -> usize {
    let mut count = 0;
    for (class, _) in target.iter() {
        if get_amount(assets, class) == 0 {
            count += 1;
        }
    }
    count
}

const MAX_SEARCH_SPACE_SIZE: usize = 1000;

struct InputSelector<'a, S: UtxoStore> {
    store: &'a S,
    ignore: HashSet<UtxoRef>,
    ignore_collateral: HashSet<UtxoRef>,
}

impl<'a, S: UtxoStore> InputSelector<'a, S> {
    pub fn new(store: &'a S) -> Self {
        Self {
            store,
            ignore: HashSet::new(),
            ignore_collateral: HashSet::new(),
        }
    }

    fn pick_from_set(utxos: UtxoSet, criteria: &CanonicalQuery) -> UtxoSet {
        let target = criteria
            .min_amount
            .clone()
            .unwrap_or(CanonicalAssets::empty());

        if criteria.support_many {
            #[cfg(feature = "naive_accumulator")]
            {
                NaiveAccumulator::pick(utxos, &target)
            }
            #[cfg(not(feature = "naive_accumulator"))]
            {
                VectorAccumulator::pick(utxos, &target)
            }
        } else {
            FirstFullMatch::pick(utxos, &target)
        }
    }

    pub async fn select_collateral(
        &mut self,
        search_space: &SearchSpace,
        criteria: &CanonicalQuery,
    ) -> Result<UtxoSet, Error> {
        let refs = search_space
            .take(Some(MAX_SEARCH_SPACE_SIZE))
            .into_iter()
            .filter(|x| !self.ignore_collateral.contains(x))
            .collect();

        let utxos = self.store.fetch_utxos(refs).await?;

        // Collateral has the extra constraint that it must be a naked utxo
        // so we need to filter out any utxos that are not naked.
        //
        // TODO: this seems like a ledger specific constraint, we should somehow
        // abstract it away. Maybe as a different call in the UtxoStore trait.
        let utxos = utxos
            .into_iter()
            .filter(|x| x.assets.is_only_naked())
            .collect();

        let matched = Self::pick_from_set(utxos, criteria);

        self.ignore_collateral
            .extend(matched.iter().map(|x| x.r#ref.clone()));

        Ok(matched)
    }

    pub async fn select_input(
        &mut self,
        search_space: &SearchSpace,
        criteria: &CanonicalQuery,
    ) -> Result<UtxoSet, Error> {
        let refs = search_space
            .take(Some(MAX_SEARCH_SPACE_SIZE))
            .into_iter()
            .filter(|x| !self.ignore.contains(x))
            .collect();

        let utxos = self.store.fetch_utxos(refs).await?;

        let matched = Self::pick_from_set(utxos, criteria);

        self.ignore.extend(matched.iter().map(|x| x.r#ref.clone()));

        Ok(matched)
    }

    pub async fn select(
        &mut self,
        search_space: &SearchSpace,
        criteria: &CanonicalQuery,
    ) -> Result<UtxoSet, Error> {
        if criteria.collateral {
            self.select_collateral(search_space, criteria).await
        } else {
            self.select_input(search_space, criteria).await
        }
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

        let space = searching::narrow_search_space(utxos, &query).await?;

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

            let space = searching::narrow_search_space(&store, &criteria)
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

        let space = searching::narrow_search_space(&store, &empty_criteria)
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

        let space = searching::narrow_search_space(&store, &criteria)
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

        let space = searching::narrow_search_space(&store, &criteria)
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

            let space = searching::narrow_search_space(&store, &criteria)
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

            let space = searching::narrow_search_space(&store, &criteria)
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

            let space = searching::narrow_search_space(&store, &criteria)
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

            let space = searching::narrow_search_space(&store, &criteria)
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

            let space = searching::narrow_search_space(&store, &criteria)
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

            let space = searching::narrow_search_space(&store, &criteria)
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
    async fn test_select_same_collateral_and_input() {
        let store = mock::seed_random_memory_store(
            |_: &mock::FuzzTxoRef, x: &mock::KnownAddress, _: u64| {
                mock::utxo_with_random_amount(x, 4_000_000..5_000_000)
            },
            1..2, // exclusive range, this means only one utxo per address
        );

        let mut selector = InputSelector::new(&store);

        for address in mock::KnownAddress::everyone() {
            // we select the only utxo availabel as an input
            let criteria = new_input_query(&address, Some(1_000_000), vec![], false, false);

            let space = searching::narrow_search_space(&store, &criteria)
                .await
                .unwrap();

            let utxos = selector.select(&space, &criteria).await.unwrap();
            assert_eq!(utxos.len(), 1);

            // try to select the same utxo as collateral
            let criteria = new_input_query(&address, Some(1_000_000), vec![], false, true);

            let space = searching::narrow_search_space(&store, &criteria)
                .await
                .unwrap();

            let utxos = selector.select(&space, &criteria).await.unwrap();
            assert_eq!(utxos.len(), 1);
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

            let space = searching::narrow_search_space(&store, &query1)
                .await
                .unwrap();

            let utxos1 = selector.select(&space, &query1).await.unwrap();
            assert_eq!(utxos1.len(), 1);

            // we ask for a large amount utxo again knowing that we already selected one of
            // those
            let query2 = new_input_query(&address, Some(1_000), vec![], true, false);

            let space = searching::narrow_search_space(&store, &query2)
                .await
                .unwrap();

            let utxos2 = selector.select(&space, &query2).await.unwrap();
            assert_eq!(utxos2.len(), 0);

            // we ask for a small amount utxo knowing that we have one of those
            let query3 = new_input_query(&address, Some(100), vec![], true, false);

            let space = searching::narrow_search_space(&store, &query3)
                .await
                .unwrap();

            let utxos3 = selector.select(&space, &query3).await.unwrap();
            assert_eq!(utxos3.len(), 1);

            // we ask for a small amount utxo again knowing that we already selected one of
            // those
            let query4 = new_input_query(&address, Some(100), vec![], true, false);

            let space = searching::narrow_search_space(&store, &query4)
                .await
                .unwrap();

            let utxos4 = selector.select(&space, &query4).await.unwrap();
            assert_eq!(utxos4.len(), 0);

            // we ensure that selected utxos are not the same
            utxos1.is_disjoint(&utxos3);
        }
    }

    #[pollster::test]
    async fn test_select_by_naked_and_asset_amount() {
        let store = mock::seed_random_memory_store(
            |_: &mock::FuzzTxoRef, x: &mock::KnownAddress, sequence: u64| {
                if sequence % 2 == 0 {
                    mock::utxo_with_random_amount(x, 4_000_000..5_000_000)
                } else {
                    mock::utxo_with_random_asset(x, mock::KnownAsset::Hosky, 500..1000)
                }
            },
            2..3,
        );

        let mut selector = InputSelector::new(&store);

        let criteria = new_input_query(
            &mock::KnownAddress::Alice,
            Some(4_000_000),
            vec![(mock::KnownAsset::Hosky, 500)],
            true,
            false,
        );

        let space = searching::narrow_search_space(&store, &criteria)
            .await
            .unwrap();

        let utxos = selector.select(&space, &criteria).await.unwrap();

        assert!(utxos.len() == 2);
    }

    fn create_utxo(id: u8, ada: u64, assets: Vec<(&str, u64)>) -> Utxo {
        let mut canonical_assets = CanonicalAssets::from_naked_amount(ada as i128 * 1_000_000);

        for (name, amount) in assets {
            let policy = vec![1u8; 28];
            let asset_name = name.as_bytes().to_vec();

            canonical_assets = canonical_assets
                + CanonicalAssets::from_asset(Some(&policy), Some(&asset_name), amount as i128);
        }

        Utxo {
            address: vec![0u8; 32], // Dummy address
            assets: canonical_assets,
            r#ref: UtxoRef {
                txid: vec![id; 32], // Unique ID based on input
                index: 0,
            },
            datum: None,
            script: None,
        }
    }

    fn create_target(ada: u64, assets: Vec<(&str, u64)>) -> CanonicalAssets {
        let mut canonical_assets = CanonicalAssets::from_naked_amount(ada as i128 * 1_000_000);

        for (name, amount) in assets {
            let policy = vec![1u8; 28];
            let asset_name = name.as_bytes().to_vec();

            canonical_assets = canonical_assets
                + CanonicalAssets::from_asset(Some(&policy), Some(&asset_name), amount as i128);
        }
        canonical_assets
    }

    #[test]
    fn test_vector_accumulator() {
        // Setup UTXOs
        let u1 = create_utxo(1, 12, vec![]);
        let u2 = create_utxo(2, 8, vec![]);
        let u3 = create_utxo(3, 2, vec![("NFT1", 1)]);
        let u4 = create_utxo(4, 2, vec![("NFT2", 1), ("NFT3", 1)]);

        let search_space: UtxoSet = vec![u1.clone(), u2.clone(), u3.clone(), u4.clone()]
            .into_iter()
            .collect();

        // Case 1: (5 ADA, 1 NFT1)
        let target = create_target(5, vec![("NFT1", 1)]);
        let selected = VectorAccumulator::pick(search_space.clone(), &target);

        assert!(selected.contains(&u3), "Case 1: Should contain U3 for NFT1");
        assert!(
            !selected.contains(&u4),
            "Case 1: Should not contain U4 (unrequested assets)"
        );
        assert!(
            selected.contains(&u2) || selected.contains(&u1),
            "Case 1: Should contain a pure ADA utxo"
        );

        // Case 2: (10 ADA, 1 NFT3)
        let target = create_target(10, vec![("NFT3", 1)]);
        let selected = VectorAccumulator::pick(search_space.clone(), &target);
        assert!(selected.contains(&u4), "Case 2: Should contain U4 for NFT3");
        assert!(
            !selected.contains(&u3),
            "Case 2: Should not contain U3 (unrequested NFT1)"
        );
        assert!(
            selected.contains(&u2) || selected.contains(&u1),
            "Case 2: Should contain a pure ADA utxo"
        );

        // Case 3: (10 ADA, 1 NFT1)
        let target = create_target(10, vec![("NFT1", 1)]);
        let selected = VectorAccumulator::pick(search_space.clone(), &target);
        assert!(selected.contains(&u3), "Case 3: Should contain U3 for NFT1");
        assert!(
            selected.contains(&u2) || selected.contains(&u1),
            "Case 3: Should contain a pure ADA utxo"
        );

        // Case 4: (12 ADA, 1 NFT1)
        let target = create_target(12, vec![("NFT1", 1)]);
        let selected = VectorAccumulator::pick(search_space.clone(), &target);
        assert!(selected.contains(&u3), "Case 4: Should contain U3");
        assert!(
            selected.contains(&u2) || selected.contains(&u1),
            "Case 4: Should contain a pure ADA utxo"
        );

        // Case 5: (12 ADA, 1 NFT2)
        let target = create_target(12, vec![("NFT2", 1)]);
        let selected = VectorAccumulator::pick(search_space.clone(), &target);
        assert!(selected.contains(&u4), "Case 5: Should contain U4 for NFT2");
        assert!(
            selected.contains(&u2) || selected.contains(&u1),
            "Case 5: Should contain a pure ADA utxo"
        );
    }
}
