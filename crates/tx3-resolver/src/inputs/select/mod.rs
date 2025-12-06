use std::collections::HashSet;

use tx3_lang::{backend::UtxoStore, CanonicalAssets, Utxo, UtxoRef, UtxoSet};

use crate::inputs::{CanonicalQuery, Error, SearchSpace};

pub mod naive;
pub mod vector;

pub trait CoinSelection {
    fn pick(search_space: UtxoSet, target: &CanonicalAssets) -> HashSet<Utxo>;
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

pub struct InputSelector<'a, S: UtxoStore> {
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
                naive::NaiveAccumulator::pick(utxos, &target)
            }
            #[cfg(not(feature = "naive_accumulator"))]
            {
                vector::VectorAccumulator::pick(utxos, &target)
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
            .take(Some(super::MAX_SEARCH_SPACE_SIZE))
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
            .take(Some(super::MAX_SEARCH_SPACE_SIZE))
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

#[cfg(test)]
mod tests {
    use tx3_lang::ir;

    use crate::{inputs::narrow, mock};

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

            let space = narrow::narrow_search_space(&store, &criteria)
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

        let space = narrow::narrow_search_space(&store, &empty_criteria)
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

        let space = narrow::narrow_search_space(&store, &criteria)
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

        let space = narrow::narrow_search_space(&store, &criteria)
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

            let space = narrow::narrow_search_space(&store, &criteria)
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

            let space = narrow::narrow_search_space(&store, &criteria)
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

            let space = narrow::narrow_search_space(&store, &criteria)
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

            let space = narrow::narrow_search_space(&store, &criteria)
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

            let space = narrow::narrow_search_space(&store, &criteria)
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

            let space = narrow::narrow_search_space(&store, &criteria)
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

            let space = narrow::narrow_search_space(&store, &criteria)
                .await
                .unwrap();

            let utxos = selector.select(&space, &criteria).await.unwrap();
            assert_eq!(utxos.len(), 1);

            // try to select the same utxo as collateral
            let criteria = new_input_query(&address, Some(1_000_000), vec![], false, true);

            let space = narrow::narrow_search_space(&store, &criteria)
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

            let space = narrow::narrow_search_space(&store, &query1).await.unwrap();

            let utxos1 = selector.select(&space, &query1).await.unwrap();
            assert_eq!(utxos1.len(), 1);

            // we ask for a large amount utxo again knowing that we already selected one of
            // those
            let query2 = new_input_query(&address, Some(1_000), vec![], true, false);

            let space = narrow::narrow_search_space(&store, &query2).await.unwrap();

            let utxos2 = selector.select(&space, &query2).await.unwrap();
            assert_eq!(utxos2.len(), 0);

            // we ask for a small amount utxo knowing that we have one of those
            let query3 = new_input_query(&address, Some(100), vec![], true, false);

            let space = narrow::narrow_search_space(&store, &query3).await.unwrap();

            let utxos3 = selector.select(&space, &query3).await.unwrap();
            assert_eq!(utxos3.len(), 1);

            // we ask for a small amount utxo again knowing that we already selected one of
            // those
            let query4 = new_input_query(&address, Some(100), vec![], true, false);

            let space = narrow::narrow_search_space(&store, &query4).await.unwrap();

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

        let space = narrow::narrow_search_space(&store, &criteria)
            .await
            .unwrap();

        let utxos = selector.select(&space, &criteria).await.unwrap();

        assert!(utxos.len() == 2);
    }
}
