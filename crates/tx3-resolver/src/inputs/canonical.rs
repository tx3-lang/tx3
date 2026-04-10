//! Canonical representation of input queries.

use std::collections::HashSet;

use tx3_tir::model::v1beta0 as tir;
use tx3_tir::model::{assets::CanonicalAssets, core::UtxoRef, core::UtxoSet};

use crate::Error;

macro_rules! data_or_bail {
    ($expr:expr, bytes) => {
        $expr
            .as_bytes()
            .ok_or(Error::ExpectedData("bytes".to_string(), $expr.clone()))
    };

    ($expr:expr, number) => {
        $expr
            .as_number()
            .ok_or(Error::ExpectedData("number".to_string(), $expr.clone()))?
    };

    ($expr:expr, assets) => {
        $expr
            .as_assets()
            .ok_or(Error::ExpectedData("assets".to_string(), $expr.clone()))
    };

    ($expr:expr, utxo_refs) => {
        $expr
            .as_utxo_refs()
            .ok_or(Error::ExpectedData("utxo refs".to_string(), $expr.clone()))
    };
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CanonicalQuery {
    pub address: Option<Vec<u8>>,
    pub min_amount: Option<CanonicalAssets>,
    pub refs: HashSet<UtxoRef>,
    pub support_many: bool,
    pub collateral: bool,
}

impl std::fmt::Display for CanonicalQuery {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CanonicalQuery {{")?;

        if let Some(address) = &self.address {
            write!(f, "address: {}", hex::encode(address))?;
        }

        if let Some(min_amount) = &self.min_amount {
            write!(f, "min_amount: {}", min_amount)?;
        }

        for (i, ref_) in self.refs.iter().enumerate() {
            write!(f, "ref[{}]:{}#{}", i, hex::encode(&ref_.txid), ref_.index)?;
        }

        write!(f, "support_many: {:?}", self.support_many)?;
        write!(f, "for_collateral: {:?}", self.collateral)?;
        write!(f, "}}")
    }
}

impl TryFrom<tir::InputQuery> for CanonicalQuery {
    type Error = Error;

    fn try_from(query: tir::InputQuery) -> Result<Self, Self::Error> {
        let address = query
            .address
            .as_option()
            .map(|x| data_or_bail!(x, bytes))
            .transpose()?
            .map(Vec::from);

        let min_amount = query
            .min_amount
            .as_option()
            .map(|x| data_or_bail!(x, assets))
            .transpose()?
            .map(|x| CanonicalAssets::from(Vec::from(x)));

        let refs = query
            .r#ref
            .as_option()
            .map(|x| data_or_bail!(x, utxo_refs))
            .transpose()?
            .map(|x| HashSet::from_iter(x.iter().cloned()))
            .unwrap_or_default();

        Ok(Self {
            address,
            min_amount,
            refs,
            support_many: query.many,
            collateral: query.collateral,
        })
    }
}
