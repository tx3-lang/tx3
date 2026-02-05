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

pub type UtxoSet = HashSet<Utxo>;

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
