use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use tx3_lang::{CanonicalAssets, Utxo, UtxoSet};

use super::CoinSelection;

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

pub struct VectorAccumulator;

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

        while let Some(utxo) = super::find_first_excess_utxo(&matched, target) {
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

#[cfg(test)]
mod tests {
    use super::*;

    use tx3_lang::{UtxoRef, UtxoSet};

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

// #[cfg(test)]
// mod tests2 {
//     use super::*;
//     use proptest::prelude::*;

//     prop_compose! {
//       fn any_asset() (
//         policy in any::<Vec<u8>>(),
//         name in any::<Vec<u8>>(),
//         amount in any::<i128>(),
//       ) -> CanonicalAssets {
//         CanonicalAssets::from_defined_asset(&policy, &name, amount)
//       }
//     }

//     proptest! {
//         #[test]
//         fn selection_contains_target(asset in any_asset()) {
//             let x = CanonicalAssets::empty();
//             assert!(!x.contains_total(&asset));
//             assert!(!x.contains_some(&asset));
//         }
//     }
// }
