use std::cmp::Ordering;

use tx3_tir::model::core::{Utxo, UtxoRef};

pub fn compare_utxo_refs(a: &UtxoRef, b: &UtxoRef) -> Ordering {
    a.txid.cmp(&b.txid).then_with(|| a.index.cmp(&b.index))
}

pub fn compare_utxos_by_ref(a: &Utxo, b: &Utxo) -> Ordering {
    compare_utxo_refs(&a.r#ref, &b.r#ref)
}
