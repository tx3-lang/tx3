//! Canonical representation of input queries.

use std::collections::HashSet;

use tx3_tir::model::v1beta0 as tir;
use tx3_tir::model::{assets::CanonicalAssets, core::UtxoRef};

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

impl CanonicalQuery {
    /// Raise the query's lovelace floor to `min_lovelace`, leaving every other
    /// asset requirement untouched. A floor at or below what the query already
    /// asks for is a no-op — this only ever widens the requirement.
    pub fn raise_lovelace_floor(&mut self, min_lovelace: i128) {
        if min_lovelace <= 0 {
            return;
        }

        let current = self
            .min_amount
            .as_ref()
            .and_then(|x| x.naked_amount())
            .unwrap_or(0);

        if current >= min_lovelace {
            return;
        }

        let delta = CanonicalAssets::from_naked_amount(min_lovelace - current);

        self.min_amount = Some(match self.min_amount.take() {
            Some(existing) => existing + delta,
            None => delta,
        });
    }
}

/// Lovelace that the collateral inputs of a tx paying `fees` must hold, given
/// the ledger's collateral percentage protocol parameter: `ceil(fees * pct /
/// 100)`. The ceiling matters — the ledger rejects a collateral balance that is
/// short by even one lovelace.
pub fn required_collateral(fees: u64, percentage: u64) -> i128 {
    let required = (fees as u128 * percentage as u128).div_ceil(100);
    required as i128
}

#[cfg(test)]
mod tests {
    use super::*;

    fn naked(amount: i128) -> CanonicalAssets {
        CanonicalAssets::from_naked_amount(amount)
    }

    fn collateral_query(min_amount: Option<CanonicalAssets>) -> CanonicalQuery {
        CanonicalQuery {
            address: None,
            min_amount,
            refs: HashSet::new(),
            support_many: false,
            collateral: true,
        }
    }

    #[test]
    fn required_collateral_scales_by_percentage() {
        assert_eq!(required_collateral(200_000, 150), 300_000);
        assert_eq!(required_collateral(1_000_000, 100), 1_000_000);
        assert_eq!(required_collateral(0, 150), 0);
    }

    #[test]
    fn required_collateral_rounds_up() {
        // 3 * 150 / 100 = 4.5 — the ledger rejects a short collateral balance,
        // so the fractional lovelace has to round up.
        assert_eq!(required_collateral(3, 150), 5);
        assert_eq!(required_collateral(1, 150), 2);
    }

    #[test]
    fn raise_lovelace_floor_widens_a_short_requirement() {
        let mut query = collateral_query(Some(naked(200_000)));
        query.raise_lovelace_floor(300_000);

        assert_eq!(query.min_amount.unwrap().naked_amount(), Some(300_000));
    }

    #[test]
    fn raise_lovelace_floor_never_narrows() {
        let mut query = collateral_query(Some(naked(5_000_000)));
        query.raise_lovelace_floor(300_000);

        assert_eq!(query.min_amount.unwrap().naked_amount(), Some(5_000_000));
    }

    #[test]
    fn raise_lovelace_floor_sets_an_absent_requirement() {
        let mut query = collateral_query(None);
        query.raise_lovelace_floor(300_000);

        assert_eq!(query.min_amount.unwrap().naked_amount(), Some(300_000));
    }

    #[test]
    fn raise_lovelace_floor_is_a_noop_for_a_zero_fee() {
        let mut query = collateral_query(None);
        query.raise_lovelace_floor(0);

        assert!(query.min_amount.is_none());
    }

    #[test]
    fn raise_lovelace_floor_leaves_other_assets_alone() {
        let hosky = CanonicalAssets::from_defined_asset(b"policy", b"hosky", 42);
        let mut query = collateral_query(Some(naked(200_000) + hosky.clone()));
        query.raise_lovelace_floor(300_000);

        let out = query.min_amount.unwrap();
        assert_eq!(out.naked_amount(), Some(300_000));
        assert_eq!(out.asset_amount2(b"policy", b"hosky"), Some(42));
    }
}
