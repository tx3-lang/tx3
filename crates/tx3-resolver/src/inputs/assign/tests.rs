use std::collections::HashSet;

use proptest::prelude::*;
use tx3_tir::model::{assets::CanonicalAssets, core::UtxoRef, core::UtxoSet};

use crate::test_utils::{self, utxo, utxo_with_asset};

use crate::inputs::canonical::CanonicalQuery;
use crate::job::{QueryResolution, ResolveJob};

use super::*;

// ---------------------------------------------------------------------------
// Helpers for unit tests
// ---------------------------------------------------------------------------

fn simple_query(
    many: bool,
    collateral: bool,
    min_amount: Option<CanonicalAssets>,
) -> CanonicalQuery {
    test_utils::query(None, min_amount, HashSet::new(), many, collateral)
}

fn qr(name: &str, q: CanonicalQuery, candidates: Vec<Utxo>) -> QueryResolution {
    QueryResolution {
        name: name.to_string(),
        query: q,
        candidates,
        selection: None,
    }
}

fn make_job(queries: Vec<QueryResolution>) -> ResolveJob {
    test_utils::stub_job_with_queries(queries)
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
    let candidates = vec![utxo(1, 0, b"a", 3_000_000), utxo(2, 0, b"a", 5_000_000)];

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

    let total: i128 = result
        .iter()
        .map(|u| u.assets.naked_amount().unwrap_or(0))
        .sum();
    assert!(total >= 7_000_000);
}

#[test]
fn pick_many_returns_empty_when_insufficient() {
    let target = CanonicalAssets::from_naked_amount(100_000_000);
    let candidates = vec![utxo(1, 0, b"a", 3_000_000), utxo(2, 0, b"a", 3_000_000)];

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
        utxo(1, 0, b"a", 3_000_000),                               // has ADA only
        utxo_with_asset(2, 0, b"a", 1_000_000, policy, name, 100), // has token only
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
    let utxos: UtxoSet = HashSet::from([utxo(1, 0, b"a", 5_000_000)]).into();
    assert!(find_first_excess_utxo(&utxos, &target).is_none());
}

#[test]
fn excess_returns_none_when_exactly_covered() {
    let target = CanonicalAssets::from_naked_amount(6_000_000);
    let utxos: UtxoSet =
        HashSet::from([utxo(1, 0, b"a", 3_000_000), utxo(2, 0, b"a", 3_000_000)]).into();
    assert!(find_first_excess_utxo(&utxos, &target).is_none());
}

#[test]
fn excess_finds_removable_utxo() {
    let target = CanonicalAssets::from_naked_amount(3_000_000);
    let utxos: UtxoSet =
        HashSet::from([utxo(1, 0, b"a", 5_000_000), utxo(2, 0, b"a", 2_000_000)]).into();
    // 5M + 2M = 7M, excess = 4M, utxo(2) with 2M fits in excess
    let excess = find_first_excess_utxo(&utxos, &target);
    assert!(excess.is_some());
}

#[test]
fn excess_picks_canonical_ref_when_multiple_removable() {
    let target = CanonicalAssets::from_naked_amount(1_000_000);
    let utxos: UtxoSet =
        HashSet::from([utxo(2, 0, b"a", 3_000_000), utxo(1, 0, b"a", 3_000_000)]).into();

    let excess = find_first_excess_utxo(&utxos, &target).unwrap();
    assert_eq!(excess.r#ref.txid[0], 1);
}

// ---------------------------------------------------------------------------
// assign_all
// ---------------------------------------------------------------------------

fn selection<'a>(job: &'a ResolveJob, name: &str) -> &'a UtxoSet {
    job.input_queries
        .iter()
        .find(|qr| qr.name == name)
        .and_then(|qr| qr.selection.as_ref())
        .expect("selection must be set")
}

#[test]
fn assign_all_single_query_resolved() {
    let q = simple_query(
        false,
        false,
        Some(CanonicalAssets::from_naked_amount(1_000_000)),
    );
    let candidates = vec![utxo(1, 0, b"a", 5_000_000)];

    let mut job = make_job(vec![qr("input", q, candidates)]);
    job.assign_all().unwrap();
    assert_eq!(selection(&job, "input").len(), 1);
}

#[test]
fn assign_all_single_query_unresolved() {
    let q = simple_query(
        false,
        false,
        Some(CanonicalAssets::from_naked_amount(10_000_000)),
    );
    let candidates = vec![utxo(1, 0, b"a", 5_000_000)]; // insufficient

    let mut job = make_job(vec![qr("input", q, candidates)]);
    assert!(job.assign_all().is_err());
}

#[test]
fn assign_all_tighter_query_picks_first() {
    // "tight" has 1 candidate, "loose" has 2 — tight should pick first
    let u1 = utxo(1, 0, b"a", 5_000_000);
    let u2 = utxo(2, 0, b"a", 5_000_000);
    let target = CanonicalAssets::from_naked_amount(1_000_000);

    let tight = qr(
        "tight",
        simple_query(false, false, Some(target.clone())),
        vec![u1.clone()],
    );
    let loose = qr(
        "loose",
        simple_query(false, false, Some(target)),
        vec![u1, u2],
    );

    let mut job = make_job(vec![loose, tight]); // order shouldn't matter
    job.assign_all().unwrap();

    assert_eq!(selection(&job, "tight").len(), 1);
    assert_eq!(selection(&job, "loose").len(), 1);
    // tight got u1, loose got u2 (the only remaining)
    assert!(selection(&job, "tight").is_disjoint(selection(&job, "loose")));
}

#[test]
fn assign_all_exclusivity_second_query_fails_when_utxo_taken() {
    let u1 = utxo(1, 0, b"a", 5_000_000);
    let target = CanonicalAssets::from_naked_amount(1_000_000);

    let q1 = qr(
        "a",
        simple_query(false, false, Some(target.clone())),
        vec![u1.clone()],
    );
    let q2 = qr("b", simple_query(false, false, Some(target)), vec![u1]); // same sole candidate

    let mut job = make_job(vec![q1, q2]);
    assert!(job.assign_all().is_err());
}

#[test]
fn assign_all_collateral_has_priority_over_regular() {
    // Both want the same UTxO, collateral should win (higher priority)
    let u1 = utxo(1, 0, b"a", 5_000_000);
    let target = CanonicalAssets::from_naked_amount(1_000_000);

    let regular = qr(
        "regular",
        simple_query(false, false, Some(target.clone())),
        vec![u1.clone()],
    );
    let collateral = qr(
        "collateral",
        simple_query(false, true, Some(target)),
        vec![u1],
    );

    let mut job = make_job(vec![regular, collateral]);
    // Collateral wins the only UTxO, regular fails
    assert!(job.assign_all().is_err());
}

#[test]
fn assign_all_many_query_accumulates() {
    let target = CanonicalAssets::from_naked_amount(8_000_000);
    let candidates = vec![
        utxo(1, 0, b"a", 3_000_000),
        utxo(2, 0, b"a", 3_000_000),
        utxo(3, 0, b"a", 3_000_000),
    ];

    let mut job = make_job(vec![qr(
        "input",
        simple_query(true, false, Some(target)),
        candidates,
    )]);
    job.assign_all().unwrap();
    assert!(selection(&job, "input").len() >= 2);
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
        let q1 = qr("a", simple_query(false, false, Some(target.clone())), candidates.clone());
        let q2 = qr("b", simple_query(false, false, Some(target)), candidates);

        let mut job = make_job(vec![q1, q2]);
        let _ = job.assign_all(); // may fail, but exclusivity must still hold

        let mut all_refs: Vec<UtxoRef> = Vec::new();
        for q in &job.input_queries {
            if let Some(sel) = &q.selection {
                for u in sel.iter() {
                    all_refs.push(u.r#ref.clone());
                }
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
        let q1 = qr("a", simple_query(false, false, Some(target.clone())), candidates.clone());
        let q2 = qr("b", simple_query(true, false, Some(target)), candidates);

        let mut job = make_job(vec![q1, q2]);
        let _ = job.assign_all(); // may fail, but every query should still get a selection set

        let with_selection = job.input_queries.iter().filter(|q| q.selection.is_some()).count();
        prop_assert_eq!(with_selection, 2, "every query gets a selection");
    }
}
