use std::cmp::Ordering;
use std::collections::HashSet;

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone, Hash, PartialEq, Eq)]
pub struct UtxoRef {
    pub txid: Vec<u8>,
    pub index: u32,
}

impl UtxoRef {
    pub fn new(txid: &[u8], index: u32) -> Self {
        Self {
            txid: txid.to_vec(),
            index,
        }
    }
}

pub trait CanonicalOrd {
    fn cmp_canonical(&self, other: &Self) -> Ordering;
}

impl CanonicalOrd for UtxoRef {
    fn cmp_canonical(&self, other: &Self) -> Ordering {
        self.txid
            .cmp(&other.txid)
            .then_with(|| self.index.cmp(&other.index))
    }
}

impl std::fmt::Display for UtxoRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}#{}", hex::encode(&self.txid), self.index)
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Utxo {
    pub r#ref: UtxoRef,
    pub address: Vec<u8>,
    pub assets: super::assets::CanonicalAssets,

    // TODO: we should remove the dependency on v1beta0 here and treat the UTxO fields as data instead of expressions
    pub datum: Option<super::v1beta0::Expression>,
    pub script: Option<super::v1beta0::Expression>,
}

impl std::hash::Hash for Utxo {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.r#ref.hash(state);
    }
}

impl PartialEq for Utxo {
    fn eq(&self, other: &Self) -> bool {
        self.r#ref == other.r#ref
    }
}

impl Eq for Utxo {}

impl CanonicalOrd for Utxo {
    fn cmp_canonical(&self, other: &Self) -> Ordering {
        self.r#ref.cmp_canonical(&other.r#ref)
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Default, PartialEq, Eq)]
#[serde(transparent)]
pub struct UtxoSet(HashSet<Utxo>);

impl UtxoSet {
    pub fn new() -> Self {
        Self(HashSet::new())
    }

    pub fn iter(&self) -> std::collections::hash_set::Iter<'_, Utxo> {
        self.0.iter()
    }

    pub fn iter_sorted_by_ref(&self) -> Vec<&Utxo> {
        let mut out: Vec<_> = self.0.iter().collect();
        out.sort_by(|a, b| a.cmp_canonical(b));
        out
    }

    pub fn into_sorted_by_ref(self) -> Vec<Utxo> {
        let mut out: Vec<_> = self.0.into_iter().collect();
        out.sort_by(|a, b| a.cmp_canonical(b));
        out
    }

    pub fn refs(&self) -> HashSet<UtxoRef> {
        self.iter().map(|utxo| utxo.r#ref.clone()).collect()
    }

    pub fn refs_sorted(&self) -> Vec<UtxoRef> {
        self.iter_sorted_by_ref()
            .into_iter()
            .map(|utxo| utxo.r#ref.clone())
            .collect()
    }

    pub fn total_assets(&self) -> super::assets::CanonicalAssets {
        self.iter()
            .fold(super::assets::CanonicalAssets::empty(), |acc, x| {
                acc + x.assets.clone()
            })
    }

    pub fn first_by_ref(&self) -> Option<&Utxo> {
        self.iter_sorted_by_ref().into_iter().next()
    }
}

impl std::ops::Deref for UtxoSet {
    type Target = HashSet<Utxo>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for UtxoSet {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<HashSet<Utxo>> for UtxoSet {
    fn from(value: HashSet<Utxo>) -> Self {
        Self(value)
    }
}

impl From<UtxoSet> for HashSet<Utxo> {
    fn from(value: UtxoSet) -> Self {
        value.0
    }
}

impl FromIterator<Utxo> for UtxoSet {
    fn from_iter<T: IntoIterator<Item = Utxo>>(iter: T) -> Self {
        Self(HashSet::from_iter(iter))
    }
}

impl IntoIterator for UtxoSet {
    type Item = Utxo;
    type IntoIter = std::collections::hash_set::IntoIter<Utxo>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a UtxoSet {
    type Item = &'a Utxo;
    type IntoIter = std::collections::hash_set::Iter<'a, Utxo>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Undefined,
    Unit,
    Int,
    Bool,
    Bytes,
    Address,
    Utxo,
    UtxoRef,
    AnyAsset,
    List,
    Map,
    Custom(String),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::assets::CanonicalAssets;
    use crate::model::v1beta0::Expression;

    fn utxo(txid: u8, index: u32, amount: i128) -> Utxo {
        Utxo {
            r#ref: UtxoRef::new(&[txid], index),
            address: vec![],
            assets: CanonicalAssets::from_naked_amount(amount),
            datum: Some(Expression::None),
            script: None,
        }
    }

    #[test]
    fn utxo_set_sorted_helpers_are_canonical() {
        let set: UtxoSet = HashSet::from([utxo(3, 0, 3), utxo(1, 1, 1), utxo(1, 0, 2)]).into();

        let refs = set.refs_sorted();
        assert_eq!(refs[0], UtxoRef::new(&[1], 0));
        assert_eq!(refs[1], UtxoRef::new(&[1], 1));
        assert_eq!(refs[2], UtxoRef::new(&[3], 0));

        assert_eq!(set.first_by_ref().unwrap().r#ref, UtxoRef::new(&[1], 0));
    }

    #[test]
    fn utxo_set_total_assets_sums_values() {
        let set: UtxoSet = HashSet::from([utxo(1, 0, 2), utxo(2, 0, 3)]).into();
        assert_eq!(set.total_assets().naked_amount(), Some(5));
    }
}
