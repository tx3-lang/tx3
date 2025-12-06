//! Tx input selection algorithms

use std::collections::{BTreeMap, HashSet};

use tx3_lang::{
    applying,
    backend::{self, UtxoStore},
    ir, CanonicalAssets, Utxo, UtxoRef, UtxoSet,
};

pub use crate::inputs::narrow::SearchSpace;

mod narrow;
mod select;

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

pub struct Diagnostic {
    pub query: ir::InputQuery,
    pub utxos: UtxoSet,
    pub selected: UtxoSet,
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("expected {0}, got {1:?}")]
    ExpectedData(String, ir::Expression),

    #[error("input query too broad")]
    InputQueryTooBroad,

    #[error("input not resolved: {0}")]
    InputNotResolved(String, CanonicalQuery, SearchSpace),

    #[error("store error: {0}")]
    StoreError(#[from] backend::Error),

    #[error("apply error: {0}")]
    ApplyError(#[from] applying::Error),
}

const MAX_SEARCH_SPACE_SIZE: usize = 50;

#[derive(Debug, Clone)]
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

impl TryFrom<ir::InputQuery> for CanonicalQuery {
    type Error = Error;

    fn try_from(query: ir::InputQuery) -> Result<Self, Self::Error> {
        let address = query
            .address
            .as_option()
            .map(|x| data_or_bail!(x, bytes))
            .transpose()?
            .map(|x| Vec::from(x));

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

pub async fn resolve<T: UtxoStore>(
    tx: ir::Tx,
    utxos: &T,
) -> Result<(ir::Tx, HashSet<UtxoRef>), Error> {
    let mut all_inputs = BTreeMap::new();
    let mut all_refs = HashSet::new();

    let mut selector = select::InputSelector::new(utxos);

    for (name, query) in applying::find_queries(&tx) {
        let query = CanonicalQuery::try_from(query)?;

        let space = narrow::narrow_search_space(utxos, &query).await?;

        let utxos = selector.select(&space, &query).await?;

        if utxos.is_empty() {
            return Err(Error::InputNotResolved(name.to_string(), query, space));
        }

        for utxo in &utxos {
            all_refs.insert(utxo.r#ref.clone());
        }

        all_inputs.insert(name, utxos);
    }

    let out = applying::apply_inputs(tx, &all_inputs)?;

    Ok((out, all_refs))
}
