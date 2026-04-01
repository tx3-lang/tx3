use std::collections::HashSet;

use proptest::prelude::*;
use tx3_tir::model::{assets::CanonicalAssets, core::UtxoRef};

use crate::inputs::test_utils::{self, utxo, utxo_with_asset};

use super::*;

// ---------------------------------------------------------------------------
// Helpers for unit tests
// ---------------------------------------------------------------------------

fn simple_query(many: bool, collateral: bool, min_amount: Option<CanonicalAssets>) -> CanonicalQuery {
    test_utils::query(None, min_amount, HashSet::new(), many, collateral)
}

fn prepared(name: &str, q: CanonicalQuery, candidates: Vec<Utxo>) -> PreparedQuery {
    PreparedQuery { name: name.to_string(), query: q, candidates }
}

// ---------------------------------------------------------------------------
// pick_single
// ---------------------------------------------------------------------------

#[test]
fn pick_single_returns_first_sufficient_match() {
    let target = CanonicalAssets::from_naked_amount(3_000_000);
    let candidates = vec![
        utxo(1, 0, b"a", 2_000_000), // insufficient
        utxo(2, 0, b"a", 5_000_000), // sufficient — should be picked
        utxo(3, 0, b"a", 9_000_000), // also sufficient but later
    ];

    let result = pick_single(candidates, &target);
    assert_eq!(result.len(), 1);
    assert_eq!(result.iter().next().unwrap().r#ref.txid[0], 2);
}

#[test]
fn pick_single_returns_empty_when_none_sufficient() {
    let target = CanonicalAssets::from_naked_amount(10_000_000);
    let candidates = vec![
        utxo(1, 0, b"a", 3_000_000),
        utxo(2, 0, b"a", 5_000_000),
    ];

    let result = pick_single(candidates, &target);
    assert!(result.is_empty());
}

#[test]
fn pick_single_empty_candidates() {
    let target = CanonicalAssets::from_naked_amount(1);
    assert!(pick_single(vec![], &target).is_empty());
}

// ---------------------------------------------------------------------------
// pick_many
// ---------------------------------------------------------------------------

#[test]
fn pick_many_accumulates_until_target_met() {
    let target = CanonicalAssets::from_naked_amount(7_000_000);
    let candidates = vec![
        utxo(1, 0, b"a", 3_000_000),
        utxo(2, 0, b"a", 3_000_000),
        utxo(3, 0, b"a", 3_000_000),
    ];

    let result = pick_many(candidates, &target);
    assert!(result.len() >= 2);

    let total: i128 = result.iter().map(|u| u.assets.naked_amount().unwrap_or(0)).sum();
    assert!(total >= 7_000_000);
}

#[test]
fn pick_many_returns_empty_when_insufficient() {
    let target = CanonicalAssets::from_naked_amount(100_000_000);
    let candidates = vec![
        utxo(1, 0, b"a", 3_000_000),
        utxo(2, 0, b"a", 3_000_000),
    ];

    let result = pick_many(candidates, &target);
    assert!(result.is_empty());
}

#[test]
fn pick_many_trims_excess() {
    let target = CanonicalAssets::from_naked_amount(5_000_000);
    let candidates = vec![
        utxo(1, 0, b"a", 5_000_000), // alone covers target
        utxo(2, 0, b"a", 3_000_000), // would be excess
        utxo(3, 0, b"a", 3_000_000), // would be excess
    ];

    let result = pick_many(candidates, &target);
    assert_eq!(result.len(), 1);
}

#[test]
fn pick_many_empty_candidates() {
    let target = CanonicalAssets::from_naked_amount(1);
    assert!(pick_many(vec![], &target).is_empty());
}

#[test]
fn pick_many_with_multi_asset_target() {
    let policy = b"policy01";
    let name = b"token";
    let target = CanonicalAssets::from_naked_amount(2_000_000)
        + CanonicalAssets::from_asset(Some(policy), Some(name), 100);

    let candidates = vec![
        utxo(1, 0, b"a", 3_000_000),                                   // has ADA only
        utxo_with_asset(2, 0, b"a", 1_000_000, policy, name, 100),     // has token only
    ];

    let result = pick_many(candidates, &target);
    assert_eq!(result.len(), 2);
}

// ---------------------------------------------------------------------------
// find_first_excess_utxo
// ---------------------------------------------------------------------------

#[test]
fn excess_returns_none_for_single_utxo() {
    let target = CanonicalAssets::from_naked_amount(1_000_000);
    let utxos = HashSet::from([utxo(1, 0, b"a", 5_000_000)]);
    assert!(find_first_excess_utxo(&utxos, &target).is_none());
}

#[test]
fn excess_returns_none_when_exactly_covered() {
    let target = CanonicalAssets::from_naked_amount(6_000_000);
    let utxos = HashSet::from([
        utxo(1, 0, b"a", 3_000_000),
        utxo(2, 0, b"a", 3_000_000),
    ]);
    assert!(find_first_excess_utxo(&utxos, &target).is_none());
}

#[test]
fn excess_finds_removable_utxo() {
    let target = CanonicalAssets::from_naked_amount(3_000_000);
    let utxos = HashSet::from([
        utxo(1, 0, b"a", 5_000_000),
        utxo(2, 0, b"a", 2_000_000),
    ]);
    // 5M + 2M = 7M, excess = 4M, utxo(2) with 2M fits in excess
    let excess = find_first_excess_utxo(&utxos, &target);
    assert!(excess.is_some());
}

// ---------------------------------------------------------------------------
// assign_all
// ---------------------------------------------------------------------------

#[test]
fn assign_all_single_query_resolved() {
    let q = simple_query(false, false, Some(CanonicalAssets::from_naked_amount(1_000_000)));
    let candidates = vec![utxo(1, 0, b"a", 5_000_000)];

    let result = assign_all(vec![prepared("input", q, candidates)]);
    assert_eq!(result.len(), 1);
    assert_eq!(result[0].selection.len(), 1);
}

#[test]
fn assign_all_single_query_unresolved() {
    let q = simple_query(false, false, Some(CanonicalAssets::from_naked_amount(10_000_000)));
    let candidates = vec![utxo(1, 0, b"a", 5_000_000)]; // insufficient

    let result = assign_all(vec![prepared("input", q, candidates)]);
    assert_eq!(result.len(), 1);
    assert!(result[0].selection.is_empty());
}

#[test]
fn assign_all_tighter_query_picks_first() {
    // "tight" has 1 candidate, "loose" has 2 — tight should pick first
    let u1 = utxo(1, 0, b"a", 5_000_000);
    let u2 = utxo(2, 0, b"a", 5_000_000);
    let target = CanonicalAssets::from_naked_amount(1_000_000);

    let tight = prepared("tight", simple_query(false, false, Some(target.clone())), vec![u1.clone()]);
    let loose = prepared("loose", simple_query(false, false, Some(target)), vec![u1, u2]);

    let result = assign_all(vec![loose, tight]); // order shouldn't matter
    let by_name: std::collections::HashMap<_, _> =
        result.iter().map(|a| (a.name.as_str(), &a.selection)).collect();

    assert_eq!(by_name["tight"].len(), 1);
    assert_eq!(by_name["loose"].len(), 1);
    // tight got u1, loose got u2 (the only remaining)
    assert!(by_name["tight"].is_disjoint(by_name["loose"]));
}

#[test]
fn assign_all_exclusivity_second_query_fails_when_utxo_taken() {
    let u1 = utxo(1, 0, b"a", 5_000_000);
    let target = CanonicalAssets::from_naked_amount(1_000_000);

    let q1 = prepared("a", simple_query(false, false, Some(target.clone())), vec![u1.clone()]);
    let q2 = prepared("b", simple_query(false, false, Some(target)), vec![u1]); // same sole candidate

    let result = assign_all(vec![q1, q2]);
    let resolved: Vec<_> = result.iter().filter(|a| !a.selection.is_empty()).collect();
    let unresolved: Vec<_> = result.iter().filter(|a| a.selection.is_empty()).collect();

    assert_eq!(resolved.len(), 1);
    assert_eq!(unresolved.len(), 1);
}

#[test]
fn assign_all_collateral_has_priority_over_regular() {
    // Both want the same UTxO, collateral should win (higher priority)
    let u1 = utxo(1, 0, b"a", 5_000_000);
    let target = CanonicalAssets::from_naked_amount(1_000_000);

    let regular = prepared("regular", simple_query(false, false, Some(target.clone())), vec![u1.clone()]);
    let collateral = prepared("collateral", simple_query(false, true, Some(target)), vec![u1]);

    let result = assign_all(vec![regular, collateral]);
    let by_name: std::collections::HashMap<_, _> =
        result.iter().map(|a| (a.name.as_str(), &a.selection)).collect();

    assert_eq!(by_name["collateral"].len(), 1);
    assert!(by_name["regular"].is_empty());
}

#[test]
fn assign_all_many_query_accumulates() {
    let target = CanonicalAssets::from_naked_amount(8_000_000);
    let candidates = vec![
        utxo(1, 0, b"a", 3_000_000),
        utxo(2, 0, b"a", 3_000_000),
        utxo(3, 0, b"a", 3_000_000),
    ];

    let result = assign_all(vec![prepared("input", simple_query(true, false, Some(target)), candidates)]);
    assert_eq!(result.len(), 1);
    assert!(result[0].selection.len() >= 2);
}

// ---------------------------------------------------------------------------
// Property-based tests
// ---------------------------------------------------------------------------

proptest! {
    #[test]
    fn pick_single_returns_at_most_one(
        candidates in proptest::collection::vec(test_utils::any_utxo(), 0..10),
        target in test_utils::any_composite_asset(),
    ) {
        let result = pick_single(candidates, &target);
        prop_assert!(result.len() <= 1);
    }

    #[test]
    fn pick_single_result_covers_target(
        candidates in proptest::collection::vec(test_utils::any_utxo(), 0..10),
        target in test_utils::any_composite_asset(),
    ) {
        let result = pick_single(candidates, &target);
        if let Some(selected) = result.iter().next() {
            prop_assert!(selected.assets.contains_total(&target));
        }
    }

    #[test]
    fn pick_many_result_covers_target(
        candidates in proptest::collection::vec(test_utils::any_utxo(), 0..10),
        target in test_utils::any_composite_asset(),
    ) {
        let result = pick_many(candidates, &target);
        if !result.is_empty() {
            let total = result
                .iter()
                .fold(CanonicalAssets::empty(), |acc, u| acc + u.assets.clone());
            prop_assert!(total.contains_total(&target));
        }
    }

    #[test]
    fn assign_all_preserves_exclusivity(
        candidates in proptest::collection::vec(test_utils::any_utxo(), 1..8),
        target in test_utils::any_naked_asset(),
    ) {
        let q1 = prepared("a", simple_query(false, false, Some(target.clone())), candidates.clone());
        let q2 = prepared("b", simple_query(false, false, Some(target)), candidates);

        let result = assign_all(vec![q1, q2]);

        let mut all_refs: Vec<UtxoRef> = Vec::new();
        for a in &result {
            for u in a.selection.iter() {
                all_refs.push(u.r#ref.clone());
            }
        }

        let unique: HashSet<_> = all_refs.iter().collect();
        prop_assert_eq!(all_refs.len(), unique.len(), "UTxO refs must be exclusive across assignments");
    }

    #[test]
    fn assign_all_returns_entry_per_query(
        candidates in proptest::collection::vec(test_utils::any_utxo(), 0..5),
        target in test_utils::any_naked_asset(),
    ) {
        let q1 = prepared("a", simple_query(false, false, Some(target.clone())), candidates.clone());
        let q2 = prepared("b", simple_query(true, false, Some(target)), candidates);

        let result = assign_all(vec![q1, q2]);
        prop_assert_eq!(result.len(), 2, "every query gets an entry");
    }
}
