use crate::{coercion, ChainPoint, PParams, DEFAULT_EXTRA_FEES};
use tx3_tir::model::v1beta0 as tir;
use tx3_tir::reduce::Error;

pub fn eval_size_fees(tx: &[u8], pparams: &PParams, extra_fees: Option<u64>) -> u64 {
    tx.len() as u64 * pparams.min_fee_coefficient
        + pparams.min_fee_constant
        + extra_fees.unwrap_or(DEFAULT_EXTRA_FEES)
}

pub fn slot_to_time(slot: i128, cursor: &ChainPoint) -> i128 {
    let current_time = cursor.timestamp as i128;
    let time_diff = slot - cursor.slot as i128;
    current_time + (time_diff * 1000)
}

pub fn time_to_slot(time: i128, cursor: &ChainPoint) -> i128 {
    let current_slot = cursor.slot as i128;
    let current_time = cursor.timestamp as i128;
    let time_diff = time - current_time;
    current_slot + (time_diff / 1000)
}

// Compute min utxo lovelace according to spec
// https://cips.cardano.org/cip/CIP-55
pub fn compute_min_utxo(
    x: tir::Expression,
    tx_body: &Option<crate::TxBody>,
    coins_per_byte: i128,
) -> Result<i128, Error> {
    let index = coercion::expr_into_number(&x)?;
    let overhead = 160;

    let total_bytes = if let Some(body) = tx_body {
        let utxo = body.outputs.get(index as usize).unwrap();
        let bytes = pallas::codec::minicbor::to_vec(utxo).unwrap().len() as i128;
        bytes + overhead
    } else {
        crate::MIN_UTXO_BYTES
    };

    Ok(total_bytes * coins_per_byte)
}
