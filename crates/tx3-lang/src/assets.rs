use std::collections::HashMap;

use bincode::{Decode, Encode};
use serde::{Deserialize, Serialize};

pub type AssetPolicy = Vec<u8>;
pub type AssetName = Vec<u8>;

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssetClass {
    Naked,
    Named(AssetName),
    Defined(AssetPolicy, AssetName),
}

impl AssetClass {
    pub fn is_defined(&self) -> bool {
        matches!(self, AssetClass::Defined(_, _))
    }

    pub fn is_named(&self) -> bool {
        matches!(self, AssetClass::Named(_))
    }

    pub fn is_naked(&self) -> bool {
        matches!(self, AssetClass::Naked)
    }

    pub fn policy(&self) -> Option<&[u8]> {
        match self {
            AssetClass::Defined(policy, _) => Some(policy),
            _ => None,
        }
    }

    pub fn name(&self) -> Option<&[u8]> {
        match self {
            AssetClass::Defined(_, name) => Some(name),
            AssetClass::Named(name) => Some(name),
            _ => None,
        }
    }
}

impl std::fmt::Display for AssetClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AssetClass::Naked => write!(f, "naked")?,
            AssetClass::Named(name) => write!(f, "{}", hex::encode(name))?,
            AssetClass::Defined(policy, name) => {
                write!(f, "{}.{}", hex::encode(policy), hex::encode(name))?
            }
        }

        Ok(())
    }
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct CanonicalAssets(HashMap<AssetClass, i128>);

impl std::fmt::Display for CanonicalAssets {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CanonicalAssets {{")?;

        for (class, amount) in self.iter() {
            write!(f, "{}:{}", class, amount)?;
        }

        write!(f, "}}")?;

        Ok(())
    }
}

impl Default for CanonicalAssets {
    fn default() -> Self {
        Self::empty()
    }
}

impl std::ops::Deref for CanonicalAssets {
    type Target = HashMap<AssetClass, i128>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl CanonicalAssets {
    pub fn empty() -> Self {
        Self(HashMap::new())
    }

    pub fn from_naked_amount(amount: i128) -> Self {
        Self(HashMap::from([(AssetClass::Naked, amount)]))
    }

    pub fn from_named_asset(asset_name: &[u8], amount: i128) -> Self {
        if asset_name.is_empty() {
            return Self::from_naked_amount(amount);
        }

        Self(HashMap::from([(
            AssetClass::Named(asset_name.to_vec()),
            amount,
        )]))
    }

    pub fn from_defined_asset(policy: &[u8], asset_name: &[u8], amount: i128) -> Self {
        if policy.is_empty() {
            return Self::from_named_asset(asset_name, amount);
        }

        Self(HashMap::from([(
            AssetClass::Defined(policy.to_vec(), asset_name.to_vec()),
            amount,
        )]))
    }

    pub fn from_asset(policy: Option<&[u8]>, name: Option<&[u8]>, amount: i128) -> Self {
        match (policy, name) {
            (Some(policy), Some(name)) => Self::from_defined_asset(policy, name, amount),
            (Some(policy), None) => Self::from_defined_asset(policy, &[], amount),
            (None, Some(name)) => Self::from_named_asset(name, amount),
            (None, None) => Self::from_naked_amount(amount),
        }
    }

    pub fn naked_amount(&self) -> Option<i128> {
        self.get(&AssetClass::Naked).cloned()
    }

    pub fn asset_amount(&self, policy: &[u8], name: &[u8]) -> Option<i128> {
        self.get(&AssetClass::Defined(policy.to_vec(), name.to_vec()))
            .cloned()
    }

    pub fn contains_total(&self, other: &Self) -> bool {
        for (class, other_amount) in other.iter() {
            if *other_amount == 0 {
                continue;
            }

            if *other_amount < 0 {
                return false;
            }

            let Some(self_amount) = self.get(class) else {
                return false;
            };

            if *self_amount < 0 {
                return false;
            }

            if self_amount < other_amount {
                return false;
            }
        }

        true
    }

    pub fn contains_some(&self, other: &Self) -> bool {
        if other.is_empty() {
            return true;
        }

        if self.is_empty() {
            return false;
        }

        for (class, other_amount) in other.iter() {
            if *other_amount == 0 {
                continue;
            }

            let Some(self_amount) = self.get(class) else {
                continue;
            };

            if *self_amount > 0 {
                return true;
            }
        }

        false
    }

    pub fn is_empty(&self) -> bool {
        self.iter().all(|(_, value)| *value == 0)
    }

    pub fn is_empty_or_negative(&self) -> bool {
        for (_, value) in self.iter() {
            if *value > 0 {
                return false;
            }
        }

        true
    }

    pub fn is_only_naked(&self) -> bool {
        self.iter().all(|(x, _)| x.is_naked())
    }
}

impl From<CanonicalAssets> for HashMap<AssetClass, i128> {
    fn from(assets: CanonicalAssets) -> Self {
        assets.0
    }
}

impl IntoIterator for CanonicalAssets {
    type Item = (AssetClass, i128);
    type IntoIter = std::collections::hash_map::IntoIter<AssetClass, i128>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl std::ops::Neg for CanonicalAssets {
    type Output = Self;

    fn neg(self) -> Self {
        let mut negated = self.0;

        for (_, value) in negated.iter_mut() {
            *value = -*value;
        }

        Self(negated)
    }
}

impl std::ops::Add for CanonicalAssets {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        let mut aggregated = self.0;

        for (key, value) in other.0 {
            *aggregated.entry(key).or_default() += value;
        }

        aggregated.retain(|_, &mut value| value != 0);

        Self(aggregated)
    }
}

impl std::ops::Sub for CanonicalAssets {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        let mut aggregated = self.0;

        for (key, value) in other.0 {
            *aggregated.entry(key).or_default() -= value;
        }

        aggregated.retain(|_, &mut value| value != 0);

        Self(aggregated)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    prop_compose! {
      fn any_asset() (
        policy in any::<Vec<u8>>(),
        name in any::<Vec<u8>>(),
        amount in any::<i128>(),
      ) -> CanonicalAssets {
        CanonicalAssets::from_defined_asset(&policy, &name, amount)
      }
    }

    prop_compose! {
      fn any_positive_asset() (
        policy in any::<Vec<u8>>(),
        name in any::<Vec<u8>>(),
        amount in 1..i128::MAX,
      ) -> CanonicalAssets {
        CanonicalAssets::from_defined_asset(&policy, &name, amount)
      }
    }

    prop_compose! {
      fn any_positive_composite_asset() (
        naked_amount in 0..i128::MAX,
        defined1 in any_positive_asset(),
        defined2 in any_positive_asset(),
      ) -> CanonicalAssets {
        let naked = CanonicalAssets::from_naked_amount(naked_amount);
        let composite = naked + defined1 + defined2;
        composite
      }
    }

    proptest! {
        #[test]
        fn empty_doesnt_contain_anything(asset in any_asset()) {
            let x = CanonicalAssets::empty();
            assert!(!x.contains_total(&asset));
            assert!(!x.contains_some(&asset));
        }
    }

    proptest! {
        #[test]
        fn empty_is_contained_in_everything(asset in any_asset()) {
            let x = CanonicalAssets::empty();
            assert!(asset.contains_total(&x));
            assert!(asset.contains_some(&x));
        }
    }

    proptest! {
        #[test]
        fn add_positive_makes_it_present(asset in any_positive_asset()) {
            let x = CanonicalAssets::empty();
            let x = x + asset.clone();
            assert!(x.contains_total(&asset));
            assert!(x.contains_some(&asset));
            assert!(!x.is_empty_or_negative());
        }
    }

    proptest! {
        #[test]
        fn sub_on_empty_makes_it_negative(asset in any_positive_asset()) {
            let x = CanonicalAssets::empty();
            let x = x - asset.clone();
            assert!(!x.contains_total(&asset));
            assert!(!x.contains_some(&asset));
            assert!(x.is_empty_or_negative());
        }
    }

    proptest! {
        #[test]
        fn add_is_inverse_of_sub(original in any_asset(), subtracted in any_asset()) {
            let x = original.clone();
            let x = x - subtracted.clone();
            let x = x + subtracted.clone().clone();
            assert_eq!(x, original);
        }
    }

    proptest! {
        #[test]
        fn composite_contains_some_naked(composite in any_positive_composite_asset()) {
            assert!(composite.contains_some(&CanonicalAssets::from_naked_amount(1)));
        }
    }

    proptest! {
        #[test]
        fn composite_contains_some_composite(composite1 in any_positive_composite_asset(), composite2 in any_positive_composite_asset()) {
            assert!(composite1.contains_some(&composite2));
        }
    }
}
