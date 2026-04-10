//! Shared test utilities for the inputs module: proptest strategies and builders.

use std::collections::HashSet;

use proptest::prelude::*;
use tx3_tir::model::{
    assets::{AssetClass, CanonicalAssets},
    core::{Utxo, UtxoRef},
};

use super::canonical::CanonicalQuery;

// ---------------------------------------------------------------------------
// proptest strategies — primitives
// ---------------------------------------------------------------------------

prop_compose! {
    /// Generates a positive asset amount in a realistic range.
    /// Capped to avoid overflow in arithmetic operations (e.g. subtraction).
    pub fn any_positive_amount()(amount in 1..=i64::MAX as i128) -> i128 {
        amount
    }
}

prop_compose! {
    pub fn any_policy()(policy in any::<[u8; 32]>()) -> Vec<u8> {
        Vec::from(policy)
    }
}

prop_compose! {
    pub fn any_asset_name()(name in any::<[u8; 16]>()) -> Vec<u8> {
        Vec::from(name)
    }
}

prop_compose! {
    pub fn any_address()(addr in any::<[u8; 28]>()) -> Vec<u8> {
        Vec::from(addr)
    }
}

prop_compose! {
    pub fn any_utxo_ref()(txid in any::<[u8; 32]>(), index in 0u32..16) -> UtxoRef {
        UtxoRef { txid: Vec::from(txid), index }
    }
}

// ---------------------------------------------------------------------------
// proptest strategies — assets
// ---------------------------------------------------------------------------

prop_compose! {
    pub fn any_asset_class()(policy in any_policy(), name in any_asset_name()) -> AssetClass {
        AssetClass::Defined(policy, name)
    }
}

prop_compose! {
    pub fn any_naked_asset()(amount in any_positive_amount()) -> CanonicalAssets {
        CanonicalAssets::from_naked_amount(amount)
    }
}

prop_compose! {
    pub fn any_defined_asset()(
        policy in any_policy(),
        name in any_asset_name(),
        amount in any_positive_amount(),
    ) -> CanonicalAssets {
        CanonicalAssets::from_defined_asset(&policy, &name, amount)
    }
}

prop_compose! {
    pub fn any_composite_asset()(
        naked in any_naked_asset(),
        defined in any_defined_asset(),
    ) -> CanonicalAssets {
        naked + defined
    }
}

// ---------------------------------------------------------------------------
// proptest strategies — UTxOs
// ---------------------------------------------------------------------------

prop_compose! {
    pub fn any_utxo()(
        r#ref in any_utxo_ref(),
        address in any_address(),
        assets in any_composite_asset(),
    ) -> Utxo {
        Utxo { r#ref, address, assets, datum: None, script: None }
    }
}

prop_compose! {
    pub fn any_utxo_at(address: Vec<u8>)(
        r#ref in any_utxo_ref(),
        assets in any_composite_asset(),
    ) -> Utxo {
        Utxo { r#ref, address: address.clone(), assets, datum: None, script: None }
    }
}

prop_compose! {
    pub fn any_utxo_with(assets: CanonicalAssets)(
        r#ref in any_utxo_ref(),
        address in any_address(),
    ) -> Utxo {
        Utxo { r#ref, address, assets: assets.clone(), datum: None, script: None }
    }
}

// ---------------------------------------------------------------------------
// proptest strategies — queries
// ---------------------------------------------------------------------------

prop_compose! {
    pub fn any_query()(
        address in proptest::option::of(any_address()),
        min_amount in proptest::option::of(any_composite_asset()),
        many in any::<bool>(),
        collateral in any::<bool>(),
    ) -> CanonicalQuery {
        CanonicalQuery {
            address,
            min_amount,
            refs: HashSet::new(),
            support_many: many,
            collateral,
        }
    }
}

// ---------------------------------------------------------------------------
// Builders for hand-crafted scenarios
// ---------------------------------------------------------------------------

pub fn utxo(txid: u8, index: u32, address: &[u8], naked: i128) -> Utxo {
    Utxo {
        r#ref: UtxoRef {
            txid: vec![txid; 32],
            index,
        },
        address: address.to_vec(),
        assets: CanonicalAssets::from_naked_amount(naked),
        datum: None,
        script: None,
    }
}

pub fn utxo_with_ref(address: &[u8], naked: i128, txid: u8, index: u32) -> Utxo {
    utxo(txid, index, address, naked)
}

pub fn utxo_with_asset(
    txid: u8,
    index: u32,
    address: &[u8],
    naked: i128,
    policy: &[u8],
    name: &[u8],
    amount: i128,
) -> Utxo {
    Utxo {
        r#ref: UtxoRef {
            txid: vec![txid; 32],
            index,
        },
        address: address.to_vec(),
        assets: CanonicalAssets::from_naked_amount(naked)
            + CanonicalAssets::from_asset(Some(policy), Some(name), amount),
        datum: None,
        script: None,
    }
}

pub fn query(
    address: Option<&[u8]>,
    min_amount: Option<CanonicalAssets>,
    refs: HashSet<UtxoRef>,
    many: bool,
    collateral: bool,
) -> CanonicalQuery {
    CanonicalQuery {
        address: address.map(|a| a.to_vec()),
        min_amount,
        refs,
        support_many: many,
        collateral,
    }
}

// ---------------------------------------------------------------------------
// Mock UTxO store
// ---------------------------------------------------------------------------

use std::ops::{Deref, DerefMut, Range};

use crate::{Error, UtxoPattern, UtxoStore};

pub use chainfuzz::addresses::KnownAddress;
pub use chainfuzz::assets::KnownAsset;
pub use chainfuzz::utxos::UtxoMap;
pub use chainfuzz::utxos::{utxo_with_random_amount, utxo_with_random_asset, UtxoGenerator};
pub use chainfuzz::TxoRef as FuzzTxoRef;

fn from_fuzz_utxo(txo: &chainfuzz::TxoRef, fuzz_utxo: &chainfuzz::Utxo) -> Utxo {
    let address = fuzz_utxo.address.to_vec();

    let assets: CanonicalAssets = fuzz_utxo
        .assets
        .iter()
        .map(|x| {
            let policy = x.class.policy;
            let name = x.class.name.as_slice();
            let amount = x.amount;

            CanonicalAssets::from_asset(Some(policy.as_slice()), Some(name), amount as i128)
        })
        .fold(CanonicalAssets::empty(), |acc, x| acc + x);

    let assets = assets + CanonicalAssets::from_naked_amount(fuzz_utxo.naked_value as i128);

    Utxo {
        address,
        assets,
        r#ref: UtxoRef {
            txid: txo.tx_hash.to_vec(),
            index: txo.ordinal as u32,
        },
        datum: None,
        script: None,
    }
}

pub struct MockStore {
    utxos: chainfuzz::UtxoMap,
}

impl Default for MockStore {
    fn default() -> Self {
        Self {
            utxos: UtxoMap::new(),
        }
    }
}

impl From<chainfuzz::UtxoMap> for MockStore {
    fn from(utxos: chainfuzz::UtxoMap) -> Self {
        Self { utxos }
    }
}

impl Deref for MockStore {
    type Target = chainfuzz::UtxoMap;

    fn deref(&self) -> &Self::Target {
        &self.utxos
    }
}

impl DerefMut for MockStore {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.utxos
    }
}

impl UtxoStore for MockStore {
    async fn narrow_refs(&self, pattern: UtxoPattern<'_>) -> Result<HashSet<UtxoRef>, Error> {
        match pattern {
            UtxoPattern::ByAddress(address) => {
                let address = chainfuzz::Address::try_from(address).unwrap();

                let narrow = self
                    .utxos
                    .find_by_address(&address)
                    .map(|(x, _)| x)
                    .map(|x| UtxoRef {
                        txid: x.tx_hash.to_vec(),
                        index: x.ordinal as u32,
                    });

                Ok(HashSet::from_iter(narrow))
            }
            UtxoPattern::ByAssetPolicy(policy) => {
                let policy = chainfuzz::AssetPolicy::try_from(policy).unwrap();

                let narrow = self
                    .utxos
                    .find_by_asset_policy(&policy)
                    .map(|(x, _)| x)
                    .map(|x| UtxoRef {
                        txid: x.tx_hash.to_vec(),
                        index: x.ordinal as u32,
                    });

                Ok(HashSet::from_iter(narrow))
            }
            UtxoPattern::ByAsset(policy, name) => {
                let policy = chainfuzz::AssetPolicy::try_from(policy).unwrap();
                let name = chainfuzz::AssetName::try_from(name).unwrap();
                let asset_class = chainfuzz::AssetClass::new(policy, name);

                let narrow = self
                    .utxos
                    .find_by_asset_class(&asset_class)
                    .map(|(x, _)| x)
                    .map(|x| UtxoRef {
                        txid: x.tx_hash.to_vec(),
                        index: x.ordinal as u32,
                    });

                Ok(HashSet::from_iter(narrow))
            }
        }
    }

    async fn fetch_utxos(
        &self,
        refs: HashSet<UtxoRef>,
    ) -> Result<tx3_tir::model::core::UtxoSet, Error> {
        let utxos = refs
            .iter()
            .map(|txo| {
                let tx_hash = chainfuzz::TxHash::try_from(txo.txid.as_slice()).unwrap();
                let ordinal = txo.index as u16;
                chainfuzz::TxoRef::new(tx_hash, ordinal)
            })
            .map(|txo| (txo, self.utxos.get(&txo).unwrap()))
            .map(|(txo, fuzz_utxo)| from_fuzz_utxo(&txo, &fuzz_utxo))
            .collect::<Vec<_>>();

        Ok(utxos.into_iter().collect())
    }
}

impl MockStore {
    pub async fn by_known_address(&self, address: &KnownAddress) -> HashSet<UtxoRef> {
        let bytes = address.to_bytes();
        let pattern = UtxoPattern::ByAddress(bytes.as_slice());

        self.narrow_refs(pattern).await.unwrap()
    }

    pub async fn by_known_asset(&self, asset: &KnownAsset) -> HashSet<UtxoRef> {
        let policy = asset.policy();
        let name = asset.name();
        let pattern = UtxoPattern::ByAsset(policy.as_ref(), name.as_ref());

        self.narrow_refs(pattern).await.unwrap()
    }
}

pub fn seed_random_memory_store<G: UtxoGenerator>(
    f: G,
    utxos_per_address: Range<u64>,
) -> MockStore {
    let everyone = KnownAddress::everyone();

    let map = chainfuzz::utxos::make_custom_utxo_map(everyone, utxos_per_address, f);

    MockStore { utxos: map }
}

/// Create a minimal `ResolveJob` with only the input resolution fields
/// populated, for unit-testing the input pipeline stages in isolation.
pub fn stub_job_with_queries(queries: Vec<crate::job::QueryResolution>) -> crate::job::ResolveJob {
    use tx3_tir::encoding::AnyTir;
    use tx3_tir::model::v1beta0 as tir;
    use tx3_tir::reduce::ArgMap;

    let dummy_tir = AnyTir::V1Beta0(tir::Tx {
        fees: tir::Expression::None,
        references: vec![],
        inputs: vec![],
        outputs: vec![],
        validity: None,
        mints: vec![],
        burns: vec![],
        adhoc: vec![],
        collateral: vec![],
        signers: None,
        metadata: vec![],
    });

    let mut job = crate::job::ResolveJob::new(dummy_tir, ArgMap::new());
    job.input_queries = queries;
    job
}
