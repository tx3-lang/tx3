use std::collections::HashSet;
use tx3_tir::model::{
    assets::{AssetClass, CanonicalAssets},
    core::{Utxo, UtxoSet},
};

use crate::inputs::order::compare_utxos_by_ref;

use super::Rank;

const MISMATCH_PENALTY: f64 = 3.0;

fn magnitude_penalty(a: u32, b: u32) -> f64 {
    match (a, b) {
        // both absent → no mismatch penalty
        (0, 0) => 1.0,
        // only one side has asset → weight = λ
        (_, 0) => MISMATCH_PENALTY,
        // only one side has asset → weight = λ
        (0, _) => MISMATCH_PENALTY,
        // both present → weight = 1
        (_, _) => 1.0,
    }
}

/// Maps an i128 to a u32 using a logarithmic scale. Precision of lower range
/// is higher than the upper range.
fn map_i128_to_u32_log(x: i128) -> u32 {
    // Safety clamp
    let x = x.clamp(0, i128::MAX);

    // Convert to f64 — acceptable because of logarithmic compression.
    let xf = x as f64;
    let maxf = i128::MAX as f64;

    // Normalize in log space
    let norm = (1.0 + xf).ln() / (1.0 + maxf).ln();

    // Scale to u32 range
    (norm * (u32::MAX as f64)).round() as u32
}

trait VectorSpace {
    fn classes(&self) -> HashSet<AssetClass>;
}

trait ValueVector {
    fn as_vector(&self, all_classes: &[AssetClass]) -> Vec<u32>;

    fn distance(&self, other: &Self, all_classes: &[AssetClass]) -> u32 {
        let a = self.as_vector(all_classes);
        let b = other.as_vector(all_classes);

        assert_eq!(a.len(), b.len(), "mismatching vector lengths");

        let mut sum = 0f64;

        for i in 0..a.len() {
            let (ai, bi) = (a[i], b[i]);

            let weight = magnitude_penalty(ai, bi);
            let delta = (ai as i64 - bi as i64).abs() as f64;
            let mag_squared = weight * delta.powi(2);

            sum += mag_squared
        }

        sum.sqrt().round() as u32
    }
}

impl VectorSpace for CanonicalAssets {
    fn classes(&self) -> HashSet<AssetClass> {
        self.classes()
    }
}

impl ValueVector for CanonicalAssets {
    fn as_vector(&self, all_classes: &[AssetClass]) -> Vec<u32> {
        let mut vector = Vec::with_capacity(all_classes.len());

        for class in all_classes {
            let amount = self.asset_amount(class).unwrap_or(0);
            let amount = map_i128_to_u32_log(amount);
            vector.push(amount);
        }

        vector
    }
}

impl VectorSpace for Utxo {
    fn classes(&self) -> HashSet<AssetClass> {
        self.assets.classes()
    }
}

impl ValueVector for Utxo {
    fn as_vector(&self, all_classes: &[AssetClass]) -> Vec<u32> {
        self.assets.as_vector(all_classes)
    }
}

impl VectorSpace for HashSet<AssetClass> {
    fn classes(&self) -> HashSet<AssetClass> {
        self.clone()
    }
}

macro_rules! class_union {
    ($a:expr, $b:expr) => {{
        let a = $a.classes();
        let b = $b.classes();
        a.union(&b).cloned().collect()
    }};
    ($a:expr, $b:expr, $c:expr) => {{
        let a = $a.classes();
        let b = $b.classes();
        let c = $c.classes();
        let out: HashSet<_> = class_union!(a, b);
        let out = class_union!(out, c);
        out
    }};
}

impl VectorSpace for UtxoSet {
    fn classes(&self) -> HashSet<AssetClass> {
        let mut classes = HashSet::new();

        for utxo in self.iter() {
            classes = class_union!(classes, utxo);
        }

        classes
    }
}

pub struct VectorRanker;

impl Rank for VectorRanker {
    fn sorted_candidates(search_space: UtxoSet, target: &CanonicalAssets) -> Vec<Utxo> {
        let class_set: HashSet<_> = class_union!(search_space, target);
        let mut classes: Vec<_> = class_set.into_iter().collect();
        classes.sort();

        let mut candidates = Vec::from_iter(search_space);
        candidates.sort_by(|a, b| {
            let ad = a.assets.distance(target, &classes);
            let bd = b.assets.distance(target, &classes);
            ad.cmp(&bd).then_with(|| compare_utxos_by_ref(a, b))
        });

        candidates
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use tx3_tir::model::core::UtxoRef;

    use crate::test_utils::{any_composite_asset, any_defined_asset, any_naked_asset};

    proptest! {
        #[test]
        fn distance_is_symmetric(x in any_composite_asset(), y in any_composite_asset()) {
            let classes: Vec<_> = class_union!(x, y);
            assert_eq!(x.distance(&y, &classes), y.distance(&x, &classes));
        }
    }

    proptest! {
        #[test]
        fn distance_to_self_is_zero(x in any_composite_asset()) {
            let classes: Vec<_> = x.classes().into_iter().collect();
            assert_eq!(x.distance(&x, &classes), 0);
        }
    }

    proptest! {
        #[test]
        fn distance_is_euclidean(a in any_defined_asset(), b in any_defined_asset()) {
            let classes: Vec<_> = class_union!(a, b);

            let distance = a.distance(&b, &classes);

            let vec_a = a.as_vector(&classes);
            let vec_b = b.as_vector(&classes);

            if classes.len() > 1 {
                let delta_x = (vec_a[0] as f64 - vec_b[0] as f64).abs() * MISMATCH_PENALTY;
                let delta_y = (vec_a[1] as f64 - vec_b[1] as f64).abs() * MISMATCH_PENALTY;
                let expected = f64::hypot(delta_x, delta_y).round() as u32;
                approx::assert_abs_diff_eq!(distance, expected, epsilon = 1);
            } else {
                let expected = (vec_a[0] as i64 - vec_b[0] as i64).abs() as u32;
                approx::assert_abs_diff_eq!(distance, expected, epsilon = 1);
            }
        }
    }

    proptest! {
        #[test]
        fn extra_asset_is_penalized(target in any_naked_asset(), naked in any_naked_asset(), extra in any_defined_asset()) {
            let classes: Vec<_> = class_union!(target, naked, extra);
            let a = naked.clone();
            let b = naked + extra;
            let distance_a = target.distance(&a, &classes);
            let distance_b = target.distance(&b, &classes);
            assert!(distance_a < distance_b);
        }
    }

    proptest! {
        #[test]
        fn missing_asset_is_penalized(target in any_defined_asset(), naked in any_naked_asset()) {
            let classes: Vec<_> = class_union!(target.clone(), naked.clone());
            let a = target.clone() + naked.clone();
            let b = naked.clone();
            let distance_a = target.distance(&a, &classes);
            let distance_b = target.distance(&b, &classes);

            assert!(distance_a <= distance_b);
        }
    }

    #[test]
    fn equal_distance_candidates_use_canonical_ref_tiebreaker() {
        let target = CanonicalAssets::from_naked_amount(1_000);

        let a = Utxo {
            r#ref: UtxoRef::new(&[2], 0),
            address: vec![],
            assets: CanonicalAssets::from_naked_amount(1_000),
            datum: None,
            script: None,
        };

        let b = Utxo {
            r#ref: UtxoRef::new(&[1], 0),
            address: vec![],
            assets: CanonicalAssets::from_naked_amount(1_000),
            datum: None,
            script: None,
        };

        let pool: UtxoSet = HashSet::from([a, b]);
        let ranked = VectorRanker::sorted_candidates(pool, &target);

        assert_eq!(ranked.len(), 2);
        assert_eq!(ranked[0].r#ref.txid, vec![1]);
        assert_eq!(ranked[1].r#ref.txid, vec![2]);
    }
}
