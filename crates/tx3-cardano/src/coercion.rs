use std::str::FromStr as _;

use pallas::{codec::utils::Int, ledger::primitives::conway as primitives};
use tx3_lang::ir;

use crate::{Error, Network};

pub fn string_into_address(value: &str) -> Result<pallas::ledger::addresses::Address, Error> {
    pallas::ledger::addresses::Address::from_str(value)
        .map_err(|_| Error::CoerceError(value.to_string(), "Address".to_string()))
}

pub fn bytes_into_address(value: &[u8]) -> Result<pallas::ledger::addresses::Address, Error> {
    pallas::ledger::addresses::Address::from_bytes(value)
        .map_err(|_| Error::CoerceError(hex::encode(value), "Address".to_string()))
}

pub fn policy_into_address(
    policy: &[u8],
    network: Network,
) -> Result<pallas::ledger::addresses::Address, Error> {
    let policy = primitives::Hash::from(policy);

    let network = match network {
        primitives::NetworkId::Testnet => pallas::ledger::addresses::Network::Testnet,
        primitives::NetworkId::Mainnet => pallas::ledger::addresses::Network::Mainnet,
    };

    let address = pallas::ledger::addresses::ShelleyAddress::new(
        network,
        pallas::ledger::addresses::ShelleyPaymentPart::Script(policy),
        pallas::ledger::addresses::ShelleyDelegationPart::Null,
    );

    Ok(address.into())
}

pub fn expr_into_number(expr: &ir::Expression) -> Result<i128, Error> {
    match expr {
        ir::Expression::Number(x) => Ok(*x),
        ir::Expression::Assets(x) if x.len() == 1 => expr_into_number(&x[0].amount),
        _ => Err(Error::CoerceError(
            format!("{expr:?}"),
            "Number".to_string(),
        )),
    }
}

pub fn expr_into_metadatum(
    expr: &ir::Expression,
) -> Result<pallas::ledger::primitives::alonzo::Metadatum, Error> {
    match expr {
        ir::Expression::Number(x) => Ok(pallas::ledger::primitives::alonzo::Metadatum::Int(
            Int::from(*x as i64),
        )),
        ir::Expression::String(x) => Ok(pallas::ledger::primitives::alonzo::Metadatum::Text(
            x.clone(),
        )),
        ir::Expression::Bytes(x) => Ok(pallas::ledger::primitives::alonzo::Metadatum::Bytes(
            primitives::Bytes::from(x.clone()),
        )),
        _ => Err(Error::CoerceError(
            format!("{expr:?}"),
            "Metadatum".to_string(),
        )),
    }
}

pub fn expr_into_utxo_refs(expr: &ir::Expression) -> Result<Vec<tx3_lang::UtxoRef>, Error> {
    match expr {
        ir::Expression::UtxoRefs(x) => Ok(x.clone()),
        ir::Expression::UtxoSet(x) => Ok(x.iter().map(|x| x.r#ref.clone()).collect()),
        ir::Expression::String(x) => {
            let (raw_txid, raw_output_ix) = x.split_once("#").expect("Invalid utxo ref");
            Ok(vec![tx3_lang::UtxoRef {
                txid: hex::decode(raw_txid).expect("Invalid hex txid"),
                index: raw_output_ix.parse().expect("Invalid output index"),
            }])
        }
        _ => Err(Error::CoerceError(
            format!("{expr:?}"),
            "UtxoRefs".to_string(),
        )),
    }
}

pub fn expr_into_assets(ir: &ir::Expression) -> Result<Vec<ir::AssetExpr>, Error> {
    match ir {
        ir::Expression::Assets(x) => Ok(x.clone()),
        _ => Err(Error::CoerceError(
            format!("{ir:?}"),
            "Assets".to_string(),
        )),
    }
}

pub fn address_into_stake_credential(
    address: &pallas::ledger::addresses::Address,
) -> Result<primitives::StakeCredential, Error> {
    match address {
        pallas::ledger::addresses::Address::Shelley(x) => match x.delegation() {
            pallas::ledger::addresses::ShelleyDelegationPart::Key(x) => {
                Ok(primitives::StakeCredential::AddrKeyhash(*x))
            }
            pallas::ledger::addresses::ShelleyDelegationPart::Script(x) => {
                Ok(primitives::StakeCredential::ScriptHash(*x))
            }
            _ => Err(Error::CoerceError(
                format!("{address:?}"),
                "StakeCredential".to_string(),
            )),
        },
        pallas::ledger::addresses::Address::Stake(x) => match x.payload() {
            pallas::ledger::addresses::StakePayload::Stake(x) => {
                Ok(primitives::StakeCredential::AddrKeyhash(*x))
            }
            pallas::ledger::addresses::StakePayload::Script(x) => {
                Ok(primitives::StakeCredential::ScriptHash(*x))
            }
        },
        _ => Err(Error::CoerceError(
            format!("{address:?}"),
            "StakeCredential".to_string(),
        )),
    }
}

pub fn expr_into_reward_account(
    expr: &ir::Expression,
    network: Network,
) -> Result<primitives::RewardAccount, Error> {
    let address = expr_into_address(expr, network)?;

    let hash_bytes = match address {
        pallas::ledger::addresses::Address::Shelley(x) => x.delegation().to_vec(),
        pallas::ledger::addresses::Address::Stake(x) => x.to_vec(),
        _ => return Err(Error::NoStakeAccount),
    };

    Ok(primitives::RewardAccount::from(hash_bytes))
}

pub fn expr_into_stake_credential(
    expr: &ir::Expression,
    network: Network,
) -> Result<primitives::StakeCredential, Error> {
    let address = expr_into_address(expr, network)?;
    address_into_stake_credential(&address)
}

pub fn expr_into_address(
    expr: &ir::Expression,
    network: Network,
) -> Result<pallas::ledger::addresses::Address, Error> {
    match expr {
        ir::Expression::Address(x) => bytes_into_address(x),
        ir::Expression::Hash(x) => policy_into_address(x, network),
        ir::Expression::Bytes(x) => bytes_into_address(x),
        ir::Expression::String(x) => string_into_address(x),
        ir::Expression::EvalCompiler(x) => match x.as_ref() {
            ir::CompilerOp::BuildScriptAddress(x) => {
                let hash: primitives::Hash<28> = expr_into_hash(x)?;
                policy_into_address(hash.as_ref(), network)
            }
            _ => Err(Error::CoerceError(
                format!("{expr:?}"),
                "Address".to_string(),
            )),
        },
        _ => Err(Error::CoerceError(
            format!("{expr:?}"),
            "Address".to_string(),
        )),
    }
}

pub fn address_into_keyhash(
    address: &pallas::ledger::addresses::Address,
) -> Result<primitives::AddrKeyhash, Error> {
    let pallas::ledger::addresses::Address::Shelley(address) = address else {
        return Err(Error::CoerceError(
            format!("{address:?}"),
            "Shelley address".to_string(),
        ));
    };

    match address.payment() {
        pallas::ledger::addresses::ShelleyPaymentPart::Key(x) => Ok(*x),
        pallas::ledger::addresses::ShelleyPaymentPart::Script(x) => Ok(*x),
    }
}

pub fn expr_into_address_keyhash(expr: &ir::Expression) -> Result<primitives::AddrKeyhash, Error> {
    match expr {
        ir::Expression::Bytes(x) => Ok(primitives::AddrKeyhash::from(x.as_slice())),
        ir::Expression::Address(x) => {
            let address = bytes_into_address(x)?;
            address_into_keyhash(&address)
        }
        _ => Err(Error::CoerceError(
            format!("{expr:?}"),
            "AddrKeyhash".to_string(),
        )),
    }
}

pub fn expr_into_bytes(ir: &ir::Expression) -> Result<primitives::Bytes, Error> {
    match ir {
        ir::Expression::Bytes(x) => Ok(primitives::Bytes::from(x.clone())),
        ir::Expression::String(s) => Ok(primitives::Bytes::from(s.as_bytes().to_vec())),
        _ => Err(Error::CoerceError(format!("{ir:?}"), "Bytes".to_string())),
    }
}

pub fn expr_into_hash<const SIZE: usize>(
    ir: &ir::Expression,
) -> Result<primitives::Hash<SIZE>, Error> {
    match ir {
        ir::Expression::Bytes(x) => Ok(primitives::Hash::from(x.as_slice())),
        ir::Expression::Hash(x) => Ok(primitives::Hash::from(x.as_slice())),
        _ => Err(Error::CoerceError(format!("{ir:?}"), "Hash".to_string())),
    }
}
