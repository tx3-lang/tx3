use std::collections::HashSet;
use tx3_tir::model::{
    assets::{AssetClass, CanonicalAssets},
    core::{Utxo, UtxoSet},
};

use super::CoinSelection;

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

pub struct VectorSelector;

impl VectorSelector {
    fn sort_candidates(search_space: UtxoSet, target: &CanonicalAssets) -> Vec<Utxo> {
        let classes: Vec<_> = class_union!(search_space, target);

        let mut candidates = Vec::from_iter(search_space);
        candidates.sort_by_cached_key(|utxo| utxo.assets.distance(&target, &classes));
        candidates.reverse();

        candidates
    }
}

impl CoinSelection for VectorSelector {
    fn pick_many(search_space: UtxoSet, target: &CanonicalAssets) -> HashSet<Utxo> {
        let mut matched = HashSet::new();
        let mut pending = target.clone();

        let candidates = Self::sort_candidates(search_space, target);

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

        while let Some(utxo) = super::find_first_excess_utxo(&matched, target) {
            matched.remove(&utxo);
        }

        matched
    }

    fn pick_single(search_space: UtxoSet, target: &CanonicalAssets) -> UtxoSet {
        let candidates = Self::sort_candidates(search_space, target);

        let first_match = candidates
            .iter()
            .filter(|utxo| utxo.assets.contains_total(target))
            .next();

        HashSet::from_iter(first_match.into_iter().cloned())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    prop_compose! {
      fn any_positive_amount() (
        amount in 1..=i128::MAX,
      ) -> i128 {
        amount as i128
      }
    }

    prop_compose! {
      fn any_policy() (
        policy in any::<[u8; 32]>(),
      ) -> Vec<u8> {
        Vec::from(policy)
      }
    }

    prop_compose! {
      fn any_name() (
        name in any::<[u8; 16]>(),
      ) -> Vec<u8> {
        Vec::from(name)
      }
    }

    prop_compose! {
      fn any_asset_class() (
        policy in any_policy(),
        name in any_name(),
      ) -> AssetClass {
        AssetClass::Defined(policy, name)
      }
    }

    prop_compose! {
      fn any_defined_asset() (
        policy in any_policy(),
        name in any_name(),
        amount in any_positive_amount(),
      ) -> CanonicalAssets {
        CanonicalAssets::from_defined_asset(&policy, &name, amount)
      }
    }

    prop_compose! {
      fn any_naked_asset() (
        amount in any_positive_amount(),
      ) -> CanonicalAssets {
        CanonicalAssets::from_naked_amount(amount)
      }
    }

    prop_compose! {
      fn any_composite_asset() (
        naked in any_naked_asset(),
        defined in any_defined_asset(),
      ) -> CanonicalAssets {
        naked + defined
      }
    }

    pub struct AssetConstraint {
        range: std::ops::RangeInclusive<i128>,
        class: AssetClass,
    }

    prop_compose! {
      fn any_constrained_asset(constraint: AssetConstraint) (
        amount in constraint.range,
      ) -> CanonicalAssets {
        CanonicalAssets::from_class_and_amount(constraint.class.clone(), amount)
      }
    }

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
}
