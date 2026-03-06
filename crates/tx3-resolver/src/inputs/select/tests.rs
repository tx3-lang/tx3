use chainfuzz::utxos::UtxoBuilder;
use tx3_tir::model::{assets::CanonicalAssets, v1beta0 as tir};

use crate::{inputs::narrow, mock};

use super::*;

pub fn new_input_query(
    address: &mock::KnownAddress,
    naked_amount: Option<u64>,
    other_assets: Vec<(mock::KnownAsset, u64)>,
    many: bool,
    collateral: bool,
) -> CanonicalQuery {
    let naked_asset = naked_amount.map(|x| tir::AssetExpr {
        policy: tir::Expression::None,
        asset_name: tir::Expression::None,
        amount: tir::Expression::Number(x as i128),
    });

    let other_assets: Vec<tir::AssetExpr> = other_assets
        .into_iter()
        .map(|(asset, amount)| tir::AssetExpr {
            policy: tir::Expression::Bytes(asset.policy().as_slice().to_vec()),
            asset_name: tir::Expression::Bytes(asset.name().to_vec()),
            amount: tir::Expression::Number(amount as i128),
        })
        .collect();

    let all_assets = naked_asset.into_iter().chain(other_assets).collect();

    tir::InputQuery {
        address: tir::Expression::Address(address.to_bytes()),
        min_amount: tir::Expression::Assets(all_assets),
        r#ref: tir::Expression::None,
        many,
        collateral,
    }
    .try_into()
    .unwrap()
}

async fn select_single<S: UtxoStore>(
    store: &S,
    name: &str,
    criteria: &CanonicalQuery,
) -> UtxoSet {
    let mut selector = InputSelector::new(store);
    selector
        .add(name.to_string(), criteria.clone())
        .await
        .unwrap();

    match selector.select_all() {
        Ok(selected) => selected.get(name).cloned().unwrap_or_default(),
        Err(Error::InputNotResolved(..)) => UtxoSet::default(),
        Err(e) => panic!("unexpected error: {e:?}"),
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

    for subject in mock::KnownAddress::everyone() {
        let criteria = new_input_query(&subject, None, vec![], false, false);

        let utxos = select_single(&store, "q", &criteria).await;

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

    let empty_criteria = tir::InputQuery {
        address: tir::Expression::None,
        min_amount: tir::Expression::None,
        r#ref: tir::Expression::None,
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

    let criteria = new_input_query(&mock::KnownAddress::Alice, None, vec![], true, false);

    let utxos = select_single(&store, "q", &criteria).await;
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

    let criteria = new_input_query(
        &mock::KnownAddress::Alice,
        Some(6_000_000),
        vec![],
        false,
        false,
    );

    let utxos = select_single(&store, "q", &criteria).await;
    assert!(utxos.is_empty());

    let criteria = new_input_query(
        &mock::KnownAddress::Alice,
        Some(4_000_000),
        vec![],
        false,
        false,
    );

    let utxos = select_single(&store, "q", &criteria).await;

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

        let utxos = select_single(&store, "q", &criteria).await;
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

        let utxos = select_single(&store, "q", &criteria).await;
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

        let utxos = select_single(&store, "q", &criteria).await;
        assert!(utxos.is_empty());

        // test negative case where we ask for a different asset

        let criteria = new_input_query(
            &address,
            None,
            vec![(mock::KnownAsset::Snek, 500)],
            false,
            false,
        );

        let utxos = select_single(&store, "q", &criteria).await;
        assert!(utxos.is_empty());

        // test positive case where we ask for the present asset and amount within range

        let criteria = new_input_query(
            &address,
            None,
            vec![(mock::KnownAsset::Hosky, 500)],
            false,
            false,
        );

        let utxos = select_single(&store, "q", &criteria).await;
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

    for address in mock::KnownAddress::everyone() {
        let criteria = new_input_query(&address, Some(1_000_000), vec![], false, true);

        let utxos = select_single(&store, "q", &criteria).await;

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

    for address in mock::KnownAddress::everyone() {
        let input_query = new_input_query(&address, Some(1_000_000), vec![], false, false);
        let collateral_query = new_input_query(&address, Some(1_000_000), vec![], false, true);

        let mut selector = InputSelector::new(&store);
        selector
            .add("input".to_string(), input_query)
            .await
            .unwrap();
        selector
            .add("collateral".to_string(), collateral_query)
            .await
            .unwrap();

        let selected = selector.select_all();

        // With only one UTxO, collateral (more constrained) gets it, input gets nothing
        // select_all now returns an error when any query resolves empty
        assert!(selected.is_err());
    }
}

#[pollster::test]
async fn test_select_all_exclusive_assignments() {
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
        let large_query = new_input_query(&address, Some(1_000), vec![], false, false);
        let small_query = new_input_query(&address, Some(100), vec![], false, false);

        let mut selector = InputSelector::new(&store);
        selector
            .add("large".to_string(), large_query)
            .await
            .unwrap();
        selector
            .add("small".to_string(), small_query)
            .await
            .unwrap();

        let selected = selector.select_all().unwrap();

        let large_utxos = selected.get("large").cloned().unwrap_or_default();
        let small_utxos = selected.get("small").cloned().unwrap_or_default();

        assert_eq!(large_utxos.len(), 1);
        assert_eq!(small_utxos.len(), 1);
        assert!(large_utxos.is_disjoint(&small_utxos));
    }
}

#[pollster::test]
async fn test_select_all_competing_queries() {
    let store = mock::seed_random_memory_store(
        |_: &mock::FuzzTxoRef, x: &mock::KnownAddress, seq: u64| {
            if seq % 2 == 0 {
                UtxoBuilder::new()
                    .with_address(x)
                    .with_naked_value(4_000_000)
                    .with_random_asset(mock::KnownAsset::Hosky, 500..501)
                    .build()
            } else {
                UtxoBuilder::new()
                    .with_address(x)
                    .with_naked_value(4_000_000)
                    .with_random_asset(mock::KnownAsset::Snek, 500..501)
                    .build()
            }
        },
        2..3,
    );

    let address = mock::KnownAddress::Alice;
    let asset_query =
        new_input_query(&address, None, vec![(mock::KnownAsset::Hosky, 1)], false, false);
    let naked_query = new_input_query(&address, Some(1), vec![], false, false);

    let mut selector = InputSelector::new(&store);
    selector
        .add("asset".to_string(), asset_query)
        .await
        .unwrap();
    selector
        .add("naked".to_string(), naked_query)
        .await
        .unwrap();

    let selected = selector.select_all().unwrap();

    let asset_utxos = selected.get("asset").cloned().unwrap_or_default();
    let naked_utxos = selected.get("naked").cloned().unwrap_or_default();

    assert_eq!(asset_utxos.len(), 1);
    assert_eq!(naked_utxos.len(), 1);
    assert!(asset_utxos.is_disjoint(&naked_utxos));

    let target_asset = CanonicalAssets::from_asset(
        Some(mock::KnownAsset::Hosky.policy().as_ref()),
        Some(mock::KnownAsset::Hosky.name().as_ref()),
        1,
    );

    let asset_utxo = asset_utxos.iter().next().unwrap();
    assert!(asset_utxo.assets.contains_total(&target_asset));
}

#[pollster::test]
async fn test_select_all_competing_queries_no_solution() {
    let store = mock::seed_random_memory_store(
        |_: &mock::FuzzTxoRef, x: &mock::KnownAddress, _seq: u64| {
            UtxoBuilder::new()
                .with_address(x)
                .with_naked_value(4_000_000)
                .with_random_asset(mock::KnownAsset::Hosky, 500..501)
                .build()
        },
        1..2,
    );

    let address = mock::KnownAddress::Alice;
    let query_a =
        new_input_query(&address, None, vec![(mock::KnownAsset::Hosky, 1)], false, false);
    let query_b =
        new_input_query(&address, None, vec![(mock::KnownAsset::Hosky, 1)], false, false);

    let mut selector = InputSelector::new(&store);
    selector.add("a".to_string(), query_a).await.unwrap();
    selector.add("b".to_string(), query_b).await.unwrap();

    // With only one UTxO, the second query must fail
    let result = selector.select_all();
    assert!(result.is_err());
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

    let criteria = new_input_query(
        &mock::KnownAddress::Alice,
        Some(4_000_000),
        vec![(mock::KnownAsset::Hosky, 500)],
        true,
        false,
    );

    let utxos = select_single(&store, "q", &criteria).await;

    assert!(utxos.len() == 2);
}
