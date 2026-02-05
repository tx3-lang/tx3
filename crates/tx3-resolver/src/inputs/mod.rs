//! Tx input selection algorithms

use std::collections::{BTreeMap, HashSet};

use tx3_tir::encoding::AnyTir;
use tx3_tir::model::core::UtxoSet;
use tx3_tir::model::v1beta0 as tir;
use tx3_tir::model::{assets::CanonicalAssets, core::UtxoRef};

pub use crate::inputs::narrow::SearchSpace;
use crate::{Error, UtxoStore};

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
    pub query: tir::InputQuery,
    pub utxos: UtxoSet,
    pub selected: UtxoSet,
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

pub async fn resolve<T: UtxoStore>(tx: AnyTir, utxos: &T) -> Result<AnyTir, Error> {
    let mut all_inputs = BTreeMap::new();

    let mut selector = select::InputSelector::new(utxos);

    for (name, query) in tx3_tir::reduce::find_queries(&tx) {
        let query = CanonicalQuery::try_from(query)?;

        let space = narrow::narrow_search_space(utxos, &query).await?;

        let utxos = selector.select(&space, &query).await?;

        if utxos.is_empty() {
            return Err(Error::InputNotResolved(name.to_string(), query, space));
        }

        all_inputs.insert(name, utxos);
    }

    let out = tx3_tir::reduce::apply_inputs(tx, &all_inputs)?;

    Ok(out)
}
