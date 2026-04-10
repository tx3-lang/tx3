//! Constraint filters for UTxO candidate selection.
//!
//! Hard constraints must be satisfied by each UTxO individually (address, refs,
//! collateral). Aggregate constraints (asset amounts) can be met by combining
//! multiple UTxOs when the query supports it.

use tx3_tir::model::{assets::CanonicalAssets, core::Utxo};

use crate::inputs::canonical::CanonicalQuery;

fn matches_collateral_constraint(query: &CanonicalQuery, utxo: &Utxo) -> bool {
    !query.collateral || utxo.assets.is_only_naked()
}

fn matches_address_constraint(query: &CanonicalQuery, utxo: &Utxo) -> bool {
    query
        .address
        .as_ref()
        .map_or(true, |addr| utxo.address == *addr)
}

fn matches_ref_constraint(query: &CanonicalQuery, utxo: &Utxo) -> bool {
    query.refs.is_empty() || query.refs.contains(&utxo.r#ref)
}

/// Checks non-negotiable constraints that a UTxO must satisfy individually.
/// These cannot be met by combining multiple UTxOs.
pub fn matches_hard_constraints(query: &CanonicalQuery, utxo: &Utxo) -> bool {
    matches_collateral_constraint(query, utxo)
        && matches_address_constraint(query, utxo)
        && matches_ref_constraint(query, utxo)
}

/// Checks asset-amount constraints that support aggregation across UTxOs.
/// For single inputs, each UTxO must fully cover the target (`contains_total`).
/// For many inputs, partial coverage is enough (`contains_some`).
pub fn matches_aggregate_constraints(
    query: &CanonicalQuery,
    utxo: &Utxo,
    target: &CanonicalAssets,
) -> bool {
    if query.support_many {
        utxo.assets.contains_some(target)
    } else {
        utxo.assets.contains_total(target)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use proptest::prelude::*;
    use tx3_tir::model::{assets::CanonicalAssets, core::UtxoRef};

    use crate::inputs::test_utils::{
        self, any_address, any_utxo, query, utxo, utxo_with_asset, utxo_with_ref,
    };

    use super::*;

    // -- hard constraints: scenario tests --

    #[test]
    fn hard_no_constraints_matches_anything() {
        let q = query(None, None, HashSet::new(), false, false);
        let u = utxo(0, 0, b"alice", 5_000_000);
        assert!(matches_hard_constraints(&q, &u));
    }

    #[test]
    fn hard_address_match() {
        let q = query(Some(b"alice"), None, HashSet::new(), false, false);
        assert!(matches_hard_constraints(&q, &utxo(0, 0, b"alice", 1)));
        assert!(!matches_hard_constraints(&q, &utxo(0, 0, b"bob", 1)));
    }

    #[test]
    fn hard_ref_match() {
        let target_ref = UtxoRef {
            txid: vec![1; 32],
            index: 0,
        };
        let q = query(
            None,
            None,
            HashSet::from([target_ref.clone()]),
            false,
            false,
        );
        assert!(matches_hard_constraints(
            &q,
            &utxo_with_ref(b"alice", 1, 1, 0)
        ));
        assert!(!matches_hard_constraints(
            &q,
            &utxo_with_ref(b"alice", 1, 2, 0)
        ));
    }

    #[test]
    fn hard_collateral_requires_naked() {
        let q = query(Some(b"alice"), None, HashSet::new(), false, true);
        let naked = utxo(0, 0, b"alice", 5_000_000);
        let with_token = utxo_with_asset(0, 0, b"alice", 5_000_000, b"policy", b"name", 100);
        assert!(matches_hard_constraints(&q, &naked));
        assert!(!matches_hard_constraints(&q, &with_token));
    }

    #[test]
    fn hard_all_constraints_combined() {
        let target_ref = UtxoRef {
            txid: vec![1; 32],
            index: 0,
        };
        let q = query(
            Some(b"alice"),
            None,
            HashSet::from([target_ref]),
            false,
            true,
        );

        assert!(matches_hard_constraints(
            &q,
            &utxo_with_ref(b"alice", 1, 1, 0)
        ));
        assert!(!matches_hard_constraints(
            &q,
            &utxo_with_ref(b"bob", 1, 1, 0)
        ));
        assert!(!matches_hard_constraints(
            &q,
            &utxo_with_ref(b"alice", 1, 2, 0)
        ));
    }

    // -- aggregate constraints: scenario tests --

    #[test]
    fn aggregate_single_requires_total() {
        let target = CanonicalAssets::from_naked_amount(5_000_000);
        let q = query(None, Some(target.clone()), HashSet::new(), false, false);

        let enough = utxo(0, 0, b"alice", 5_000_000);
        let not_enough = utxo(0, 0, b"alice", 3_000_000);

        assert!(matches_aggregate_constraints(&q, &enough, &target));
        assert!(!matches_aggregate_constraints(&q, &not_enough, &target));
    }

    #[test]
    fn aggregate_many_allows_partial() {
        let target = CanonicalAssets::from_naked_amount(5_000_000);
        let q = query(None, Some(target.clone()), HashSet::new(), true, false);

        let partial = utxo(0, 0, b"alice", 3_000_000);
        assert!(matches_aggregate_constraints(&q, &partial, &target));
    }

    // -- property-based tests --

    proptest! {
        #[test]
        fn hard_address_mismatch_always_rejects(
            addr_a in any_address(),
            addr_b in any_address(),
            u in any_utxo(),
        ) {
            prop_assume!(addr_a != addr_b);

            let mut u = u;
            u.address = addr_a.clone();

            let q = query(Some(&addr_b), None, HashSet::new(), false, false);
            prop_assert!(!matches_hard_constraints(&q, &u));
        }

        #[test]
        fn hard_no_constraints_accepts_any_utxo(u in any_utxo()) {
            let q = query(None, None, HashSet::new(), false, false);
            prop_assert!(matches_hard_constraints(&q, &u));
        }

        #[test]
        fn hard_address_match_accepts_same_address(
            addr in any_address(),
            u in any_utxo(),
        ) {
            let mut u = u;
            u.address = addr.clone();
            // non-collateral so custom assets don't matter
            let q = query(Some(&addr), None, HashSet::new(), false, false);
            prop_assert!(matches_hard_constraints(&q, &u));
        }

        #[test]
        fn aggregate_total_implies_some(
            u in any_utxo(),
            target in test_utils::any_composite_asset(),
        ) {
            // if a UTxO passes contains_total, it must also pass contains_some
            if u.assets.contains_total(&target) {
                prop_assert!(u.assets.contains_some(&target));
            }
        }
    }
}
