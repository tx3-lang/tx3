//! Constraint filters for UTxO candidate selection.
//!
//! Hard constraints must be satisfied by each UTxO individually (address, refs,
//! collateral). Aggregate constraints (asset amounts) can be met by combining
//! multiple UTxOs when the query supports it.

use tx3_tir::model::{
    assets::CanonicalAssets,
    core::Utxo,
};

use crate::inputs::canonical::CanonicalQuery;

fn matches_collateral_constraint(query: &CanonicalQuery, utxo: &Utxo) -> bool {
    !query.collateral || utxo.assets.is_only_naked()
}

fn matches_address_constraint(query: &CanonicalQuery, utxo: &Utxo) -> bool {
    query.address.as_ref().map_or(true, |addr| utxo.address == *addr)
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

    use tx3_tir::model::{
        assets::CanonicalAssets,
        core::{Utxo, UtxoRef},
    };

    use crate::inputs::canonical::CanonicalQuery;

    use super::*;

    fn utxo(address: &[u8], naked: i128) -> Utxo {
        Utxo {
            r#ref: UtxoRef {
                txid: vec![0; 32],
                index: 0,
            },
            address: address.to_vec(),
            assets: CanonicalAssets::from_naked_amount(naked),
            datum: None,
            script: None,
        }
    }

    fn utxo_with_ref(address: &[u8], naked: i128, txid: u8, index: u32) -> Utxo {
        Utxo {
            r#ref: UtxoRef {
                txid: vec![txid; 32],
                index,
            },
            address: address.to_vec(),
            assets: CanonicalAssets::from_naked_amount(naked),
            datum: None,
            script: None,
        }
    }

    fn utxo_with_asset(address: &[u8], naked: i128, policy: &[u8], name: &[u8], amount: i128) -> Utxo {
        let assets = CanonicalAssets::from_naked_amount(naked)
            + CanonicalAssets::from_asset(Some(policy), Some(name), amount);

        Utxo {
            r#ref: UtxoRef {
                txid: vec![0; 32],
                index: 0,
            },
            address: address.to_vec(),
            assets,
            datum: None,
            script: None,
        }
    }

    fn query(
        address: Option<&[u8]>,
        min_amount: Option<CanonicalAssets>,
        refs: HashSet<UtxoRef>,
        many: bool,
        collateral: bool,
    ) -> CanonicalQuery {
        CanonicalQuery {
            address: address.map(|a| a.to_vec()),
            min_amount,
            refs,
            support_many: many,
            collateral,
        }
    }

    // -- hard constraints --

    #[test]
    fn hard_no_constraints_matches_anything() {
        let q = query(None, None, HashSet::new(), false, false);
        let u = utxo(b"alice", 5_000_000);
        assert!(matches_hard_constraints(&q, &u));
    }

    #[test]
    fn hard_address_match() {
        let q = query(Some(b"alice"), None, HashSet::new(), false, false);
        assert!(matches_hard_constraints(&q, &utxo(b"alice", 1)));
        assert!(!matches_hard_constraints(&q, &utxo(b"bob", 1)));
    }

    #[test]
    fn hard_ref_match() {
        let target_ref = UtxoRef { txid: vec![1; 32], index: 0 };
        let q = query(None, None, HashSet::from([target_ref.clone()]), false, false);
        assert!(matches_hard_constraints(&q, &utxo_with_ref(b"alice", 1, 1, 0)));
        assert!(!matches_hard_constraints(&q, &utxo_with_ref(b"alice", 1, 2, 0)));
    }

    #[test]
    fn hard_collateral_requires_naked() {
        let q = query(Some(b"alice"), None, HashSet::new(), false, true);
        let naked = utxo(b"alice", 5_000_000);
        let with_token = utxo_with_asset(b"alice", 5_000_000, b"policy", b"name", 100);
        assert!(matches_hard_constraints(&q, &naked));
        assert!(!matches_hard_constraints(&q, &with_token));
    }

    #[test]
    fn hard_all_constraints_combined() {
        let target_ref = UtxoRef { txid: vec![1; 32], index: 0 };
        let q = query(Some(b"alice"), None, HashSet::from([target_ref]), false, true);

        // right address, right ref, naked → pass
        assert!(matches_hard_constraints(&q, &utxo_with_ref(b"alice", 1, 1, 0)));
        // wrong address → fail
        assert!(!matches_hard_constraints(&q, &utxo_with_ref(b"bob", 1, 1, 0)));
        // wrong ref → fail
        assert!(!matches_hard_constraints(&q, &utxo_with_ref(b"alice", 1, 2, 0)));
    }

    // -- aggregate constraints --

    #[test]
    fn aggregate_single_requires_total() {
        let target = CanonicalAssets::from_naked_amount(5_000_000);
        let q = query(None, Some(target.clone()), HashSet::new(), false, false);

        let enough = utxo(b"alice", 5_000_000);
        let not_enough = utxo(b"alice", 3_000_000);

        assert!(matches_aggregate_constraints(&q, &enough, &target));
        assert!(!matches_aggregate_constraints(&q, &not_enough, &target));
    }

    #[test]
    fn aggregate_many_allows_partial() {
        let target = CanonicalAssets::from_naked_amount(5_000_000);
        let q = query(None, Some(target.clone()), HashSet::new(), true, false);

        let partial = utxo(b"alice", 3_000_000);
        assert!(matches_aggregate_constraints(&q, &partial, &target));
    }
}
