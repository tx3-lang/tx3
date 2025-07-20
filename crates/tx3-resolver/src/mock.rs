use std::collections::HashSet;
use std::ops::{Deref, DerefMut};

pub use chainfuzz::addresses::KnownAddress;
pub use chainfuzz::assets::KnownAsset;
pub use chainfuzz::utxos::UtxoMap;
pub use chainfuzz::utxos::{utxo_with_random_amount, utxo_with_random_asset, UtxoGenerator};
pub use chainfuzz::{TxoRef as FuzzTxoRef, Utxo as FuzzUtxo};

use tx3_lang::backends::{Error, UtxoPattern, UtxoStore};
use tx3_lang::{ir, Utxo, UtxoRef, UtxoSet};

fn from_fuzz_utxo(txo: &chainfuzz::TxoRef, utxo: &chainfuzz::Utxo) -> Utxo {
    let address = utxo.address.to_vec();

    let mut assets: Vec<_> = utxo
        .assets
        .iter()
        .map(|x| {
            let policy = x.class.policy.to_vec();
            let name = x.class.name.to_vec();
            let amount = x.amount;

            ir::AssetExpr {
                policy: ir::Expression::Bytes(policy),
                asset_name: ir::Expression::Bytes(name),
                amount: ir::Expression::Number(amount as i128),
            }
        })
        .collect();

    assets.push(ir::AssetExpr {
        policy: ir::Expression::None,
        asset_name: ir::Expression::None,
        amount: ir::Expression::Number(utxo.naked_value as i128),
    });

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

                let narrow = HashSet::from_iter(narrow);

                Ok(narrow)
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

                let narrow = HashSet::from_iter(narrow);

                Ok(narrow)
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

                let narrow = HashSet::from_iter(narrow);

                Ok(narrow)
            }
        }
    }

    async fn fetch_utxos(&self, refs: HashSet<UtxoRef>) -> Result<UtxoSet, Error> {
        let utxos = refs
            .iter()
            .map(|txo| {
                let tx_hash = chainfuzz::TxHash::try_from(txo.txid.as_slice()).unwrap();
                let ordinal = txo.index as u16;
                chainfuzz::TxoRef::new(tx_hash, ordinal)
            })
            .map(|txo| (txo, self.utxos.get(&txo).unwrap()))
            .map(|(txo, utxo)| from_fuzz_utxo(&txo, &utxo))
            .collect::<Vec<_>>();

        Ok(utxos.into_iter().collect())
    }
}

pub fn seed_random_memory_store<G: UtxoGenerator>(f: G) -> MockStore {
    let everyone = KnownAddress::everyone();
    let utxos_per_address = 2..4;

    let map = chainfuzz::utxos::make_custom_utxo_map(everyone, utxos_per_address, f);

    MockStore { utxos: map }
}
