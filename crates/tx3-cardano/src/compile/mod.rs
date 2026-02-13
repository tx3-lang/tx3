use std::collections::{BTreeMap, HashMap, HashSet};

use pallas::{
    codec::{
        minicbor,
        utils::{KeepRaw, NonEmptySet},
    },
    ledger::{
        primitives::{
            conway::{self as primitives, Redeemers, RedeemersKey, RedeemersValue},
            PlutusScript, TransactionInput,
        },
        traverse::ComputeHash,
    },
};

use super::*;
use tx3_tir::compile::Error;
use tx3_tir::model::core::UtxoRef;
use tx3_tir::model::v1beta0 as tir;

use crate::{
    coercion::{self, expr_into_bytes, expr_into_metadatum, expr_into_number},
    Network, PParams, PlutusVersion, EXECUTION_UNITS,
};

pub(crate) mod asset_math;
pub(crate) mod plutus_data;

use plutus_data::{IntoData as _, TryIntoData as _};

macro_rules! asset {
    ($policy:expr, $asset:expr, $amount:expr) => {{
        let mut aux = BTreeMap::new();
        aux.insert($asset, $amount);
        let mut asset = BTreeMap::new();
        asset.insert($policy, aux);
        asset
    }};
}

macro_rules! value {
    ($coin:expr, $assets:expr) => {
        pallas::ledger::primitives::conway::Value::Multiasset($coin, $assets)
    };
    ($coin:expr) => {
        pallas::ledger::primitives::conway::Value::Coin($coin)
    };
}

// fn extract_classes_from_multiasset(value: &conway::Value) -> Vec<AssetClass>
// {     let ma = match value {
//         conway::Value::Multiasset(_, ma) => ma.iter().cloned().collect(),
//         _ => vec![],
//     };

//     ma.into_iter()
//         .flat_map(|(policy, assets)| {
//             assets
//                 .iter()
//                 .map(|(name, _)| AssetClass {
//                     policy: policy.clone(),
//                     name: name.clone().into(),
//                 })
//                 .collect::<Vec<_>>()
//         })
//         .collect()
// }

fn compile_struct(ir: &tir::StructExpr) -> Result<primitives::PlutusData, Error> {
    let fields = ir
        .fields
        .iter()
        .map(compile_data_expr)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(plutus_data::constr(ir.constructor as u64, fields))
}

fn compile_data_expr(ir: &tir::Expression) -> Result<primitives::PlutusData, Error> {
    match ir {
        tir::Expression::Bytes(x) => Ok(x.as_data()),
        tir::Expression::Number(x) => Ok(x.as_data()),
        tir::Expression::Bool(x) => Ok(x.as_data()),
        tir::Expression::String(x) => Ok(x.as_str().as_data()),
        tir::Expression::Struct(x) => compile_struct(x),
        tir::Expression::Map(x) => x.try_as_data(),
        tir::Expression::Address(x) => Ok(x.as_data()),
        tir::Expression::List(x) => x.try_as_data(),
        _ => Err(Error::CoerceError(
            format!("{ir:?}"),
            "DataExpr".to_string(),
        )),
    }
}

fn compile_native_asset_for_output(
    ir: &tir::AssetExpr,
) -> Result<primitives::Multiasset<primitives::PositiveCoin>, Error> {
    let policy = coercion::expr_into_bytes(&ir.policy)?;
    let policy = primitives::Hash::from(policy.as_slice());
    let asset_name = coercion::expr_into_bytes(&ir.asset_name)?;
    let amount = coercion::expr_into_number(&ir.amount)?;
    let amount = primitives::PositiveCoin::try_from(amount as u64).unwrap();

    let asset = asset!(policy, asset_name.clone(), amount);

    Ok(asset)
}

fn compile_native_asset_for_mint(
    ir: &tir::AssetExpr,
    is_burn: bool,
) -> Result<primitives::Multiasset<primitives::NonZeroInt>, Error> {
    let policy = coercion::expr_into_bytes(&ir.policy)?;
    let policy = primitives::Hash::from(policy.as_slice());
    let asset_name = coercion::expr_into_bytes(&ir.asset_name)?;
    let amount = coercion::expr_into_number(&ir.amount)?;

    let amount = if !is_burn {
        primitives::NonZeroInt::try_from(amount as i64).unwrap()
    } else {
        primitives::NonZeroInt::try_from(-amount as i64).unwrap()
    };

    let asset = asset!(policy, asset_name.clone(), amount);

    Ok(asset)
}

fn compile_ada_value(ir: &tir::AssetExpr) -> Result<primitives::Value, Error> {
    let amount = coercion::expr_into_number(&ir.amount)?;

    Ok(value!(amount as u64))
}

fn compile_value(ir: &tir::AssetExpr) -> Result<primitives::Value, Error> {
    let amount = coercion::expr_into_number(&ir.amount)?;
    if ir.policy.is_none() {
        compile_ada_value(ir)
    } else if amount as i64 > 0 {
        let asset = compile_native_asset_for_output(ir)?;
        Ok(value!(0, asset))
    } else {
        Ok(value!(0))
    }
}

fn compile_adhoc_script(
    adhoc: &tir::AdHocDirective,
) -> Result<primitives::ScriptRef<'static>, Error> {
    let script = adhoc.data.get("script").map(expr_into_bytes).transpose()?;

    let version = adhoc
        .data
        .get("version")
        .map(coercion::expr_into_number)
        .transpose()?
        .map(|v| v as PlutusVersion)
        .unwrap_or(3);
    let script_bytes = script.unwrap().to_vec();
    let script_ref = match version {
        0 => {
            let decoded: pallas::codec::utils::KeepRaw<'_, primitives::NativeScript> =
                minicbor::decode(&script_bytes).unwrap();
            let owned_script = decoded.to_owned();
            primitives::ScriptRef::NativeScript(owned_script)
        }
        1 => {
            let script = primitives::PlutusScript::<1>(script_bytes.into());
            primitives::ScriptRef::PlutusV1Script(script)
        }
        2 => {
            let script = primitives::PlutusScript::<2>(script_bytes.into());
            primitives::ScriptRef::PlutusV2Script(script)
        }
        3 => {
            let script = primitives::PlutusScript::<3>(script_bytes.into());
            primitives::ScriptRef::PlutusV3Script(script)
        }
        _ => {
            return Err(Error::CoerceError(
                format!("{:?}", adhoc.data.get("version")),
                "Reference script version".to_string(),
            ));
        }
    };
    Ok(script_ref)
}

fn compile_output_block(
    ir: &tir::Output,
    network: Network,
) -> Result<primitives::TransactionOutput<'static>, Error> {
    let address = coercion::expr_into_address(&ir.address, network)?;

    let asset_list = coercion::expr_into_assets(&ir.amount)?;

    let values = asset_list
        .iter()
        .map(compile_value)
        .collect::<Result<Vec<_>, _>>()?;

    let value = asset_math::aggregate_values(values);

    let datum_option = ir.datum.as_option().map(compile_data_expr).transpose()?;

    let output = primitives::TransactionOutput::PostAlonzo(
        primitives::PostAlonzoTransactionOutput {
            address: address.to_vec().into(),
            value,
            datum_option: datum_option.map(|x| {
                primitives::DatumOption::Data(pallas::codec::utils::CborWrap(x.into())).into()
            }),
            script_ref: None,
        }
        .into(),
    );

    Ok(output)
}

fn compile_mint_block(tx: &tir::Tx) -> Result<Option<primitives::Mint>, Error> {
    if tx.mints.is_empty() && tx.burns.is_empty() {
        return Ok(None);
    }

    let mints: Vec<_> = tx
        .mints
        .iter()
        .map(|x| coercion::expr_into_assets(&x.amount))
        .collect::<Result<Vec<_>, _>>()?
        .iter()
        .flatten()
        .map(|x| compile_native_asset_for_mint(x, false))
        .collect::<Result<Vec<_>, _>>()?;

    let mints = asset_math::aggregate_assets(mints);

    let burns = tx
        .burns
        .iter()
        .map(|x| coercion::expr_into_assets(&x.amount))
        .collect::<Result<Vec<_>, _>>()?
        .iter()
        .flatten()
        .map(|x| compile_native_asset_for_mint(x, true))
        .collect::<Result<Vec<_>, _>>()?;

    let burns = asset_math::aggregate_assets(burns);

    let all = match (mints, burns) {
        (Some(mints), Some(burns)) => asset_math::aggregate_assets([mints, burns]),
        (Some(mints), None) => Some(mints),
        (None, Some(burns)) => Some(burns),
        (None, None) => None,
    };

    Ok(all)
}

fn compile_inputs(tx: &tir::Tx) -> Result<Vec<primitives::TransactionInput>, Error> {
    let refs = tx
        .inputs
        .iter()
        .flat_map(|x| coercion::expr_into_utxo_refs(&x.utxos))
        .flatten()
        .map(|x| primitives::TransactionInput {
            transaction_id: x.txid.as_slice().into(),
            index: x.index as u64,
        })
        .collect();

    Ok(refs)
}

fn output_has_assets(output: &Result<primitives::TransactionOutput<'static>, Error>) -> bool {
    match output {
        Ok(primitives::TransactionOutput::PostAlonzo(post_alonzo)) => match &post_alonzo.value {
            primitives::Value::Coin(amount) => *amount > 0,
            primitives::Value::Multiasset(coin, multiasset) => *coin > 0 || !multiasset.is_empty(),
        },
        _ => true,
    }
}

fn compile_outputs(
    tx: &tir::Tx,
    network: Network,
) -> Result<Vec<primitives::TransactionOutput<'static>>, Error> {
    let outputs = tx.outputs.iter().filter_map(|out| {
        let compiled = compile_output_block(out, network);

        if out.optional && !output_has_assets(&compiled) {
            return None;
        }

        let idx = out.declared_index.as_number().map(|n| n as usize);
        Some(compiled.map(|o| (idx, o)))
    });

    let cardano_outputs = tx
        .adhoc
        .iter()
        .filter(|x| x.name.as_str() == "cardano_publish")
        .map(|adhoc| {
            let idx = adhoc
                .data
                .get("declared_index")
                .and_then(|expr| expr.as_number())
                .map(|n| n as usize);

            compile_cardano_publish_directive(adhoc, network).map(|o| (idx, o))
        });

    let mut all_outputs: Vec<_> = outputs
        .chain(cardano_outputs)
        .collect::<Result<Vec<_>, _>>()?;

    all_outputs.sort_by_key(|(idx, _)| idx.unwrap_or(usize::MAX));

    Ok(all_outputs.into_iter().map(|(_, out)| out).collect())
}

pub fn compile_cardano_publish_directive(
    adhoc: &tir::AdHocDirective,
    network: Network,
) -> Result<primitives::TransactionOutput<'static>, Error> {
    let address = adhoc
        .data
        .get("to")
        .ok_or(Error::MissingExpression("output address".to_string()))?;
    let address = coercion::expr_into_address(address, network)?;

    let amount = adhoc
        .data
        .get("amount")
        .ok_or(Error::MissingExpression("output amount".to_string()))?;
    let asset_list = coercion::expr_into_assets(amount)?;
    let values = asset_list
        .iter()
        .map(compile_value)
        .collect::<Result<Vec<_>, _>>()?;
    let value = asset_math::aggregate_values(values);

    let datum_option = adhoc.data.get("datum").map(compile_data_expr).transpose()?;

    let script_ref = if let (Some(version_expr), Some(script_expr)) =
        (adhoc.data.get("version"), adhoc.data.get("script"))
    {
        // Create a synthetic adhoc directive that compile_adhoc_script can handle
        let mut script_data = std::collections::HashMap::new();
        script_data.insert("version".to_string(), version_expr.clone());
        script_data.insert("script".to_string(), script_expr.clone());

        let synthetic_adhoc = tir::AdHocDirective {
            name: "plutus_script".to_string(),
            data: script_data,
        };

        let script = compile_adhoc_script(&synthetic_adhoc)?;
        Some(pallas::codec::utils::CborWrap(script))
    } else {
        None
    };

    let output = primitives::TransactionOutput::PostAlonzo(
        primitives::PostAlonzoTransactionOutput {
            address: address.to_vec().into(),
            value,
            datum_option: datum_option.map(|x| {
                primitives::DatumOption::Data(pallas::codec::utils::CborWrap(x.into())).into()
            }),
            script_ref,
        }
        .into(),
    );

    Ok(output)
}

pub fn compile_withdrawal_directive(
    adhoc: &tir::AdHocDirective,
    network: Network,
) -> Result<(primitives::Bytes, u64), Error> {
    let credential = adhoc
        .data
        .get("credential")
        .ok_or(Error::MissingExpression(
            "withdrawal credential".to_string(),
        ))?;

    let credential = coercion::expr_into_reward_account(credential, network)?;

    let amount = adhoc
        .data
        .get("amount")
        .ok_or(Error::MissingExpression("withdrawal amount".to_string()))?;
    let amount = coercion::expr_into_number(amount)?;
    let amount = primitives::Coin::try_from(amount as u64).unwrap();

    Ok((credential, amount))
}

pub fn compile_withdrawals(
    tx: &tir::Tx,
    network: Network,
) -> Result<Option<BTreeMap<primitives::RewardAccount, primitives::Coin>>, Error> {
    let withdrawals: BTreeMap<_, _> = tx
        .adhoc
        .iter()
        .filter(|x| x.name.as_str() == "withdrawal")
        .map(|adhoc| compile_withdrawal_directive(adhoc, network))
        .collect::<Result<_, _>>()?;

    if withdrawals.is_empty() {
        Ok(None)
    } else {
        Ok(Some(withdrawals))
    }
}

fn compile_vote_delegation_certificate(
    x: &tir::AdHocDirective,
    network: Network,
) -> Result<primitives::Certificate, Error> {
    let stake = coercion::expr_into_stake_credential(&x.data["stake"], network)?;
    let drep = coercion::expr_into_bytes(&x.data["drep"])?;
    let drep = primitives::DRep::Key(drep.as_slice().into());

    Ok(primitives::Certificate::VoteDeleg(stake, drep))
}

fn compile_certs(tx: &tir::Tx, network: Network) -> Result<Vec<primitives::Certificate>, Error> {
    tx.adhoc
        .iter()
        .filter_map(|x| match x.name.as_str() {
            "vote_delegation_certificate" => {
                let cert = compile_vote_delegation_certificate(x, network);
                Some(cert)
            }
            _ => None,
        })
        .collect::<Result<Vec<_>, _>>()
}

fn compile_reference_inputs(tx: &tir::Tx) -> Result<Vec<primitives::TransactionInput>, Error> {
    let refs = tx
        .references
        .iter()
        .flat_map(coercion::expr_into_utxo_refs)
        .flatten()
        .map(|x| primitives::TransactionInput {
            transaction_id: x.txid.as_slice().into(),
            index: x.index as u64,
        })
        .collect();

    Ok(refs)
}

fn compile_collateral(tx: &tir::Tx) -> Result<Vec<TransactionInput>, Error> {
    Ok(tx
        .collateral
        .iter()
        .filter_map(|collateral| collateral.utxos.as_option())
        .flat_map(coercion::expr_into_utxo_refs)
        .flatten()
        .map(|x| primitives::TransactionInput {
            transaction_id: x.txid.as_slice().into(),
            index: x.index as u64,
        })
        .collect())
}

fn compile_required_signers(tx: &tir::Tx) -> Result<Option<primitives::RequiredSigners>, Error> {
    let Some(signers) = &tx.signers else {
        return Ok(None);
    };

    let hashes = signers
        .signers
        .iter()
        .map(coercion::expr_into_address_keyhash)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(primitives::RequiredSigners::from_vec(hashes))
}

fn compile_validity(validity: Option<&tir::Validity>) -> Result<(Option<u64>, Option<u64>), Error> {
    let since = validity
        .and_then(|v| v.since.as_option())
        .map(coercion::expr_into_number)
        .transpose()?
        .map(|n| n as u64);

    let until = validity
        .and_then(|v| v.until.as_option())
        .map(coercion::expr_into_number)
        .transpose()?
        .map(|n| n as u64);

    Ok((since, until))
}

fn compile_donation(tx: &tir::Tx) -> Result<Option<pallas::codec::utils::PositiveCoin>, Error> {
    tx.adhoc
        .iter()
        .find(|x| x.name.as_str() == "treasury_donation")
        .and_then(|donation| donation.data.get("coin"))
        .map(coercion::expr_into_number)
        .transpose()?
        .map(|amount| {
            pallas::codec::utils::PositiveCoin::try_from(amount as u64).map_err(|_| {
                Error::CoerceError(
                    format!("Invalid donation amount: {}", amount),
                    "PositiveCoin".to_string(),
                )
            })
        })
        .transpose()
}

fn compile_tx_body(
    tx: &tir::Tx,
    network: Network,
) -> Result<primitives::TransactionBody<'static>, Error> {
    let (since, until) = compile_validity(tx.validity.as_ref())?;

    let out = primitives::TransactionBody {
        inputs: compile_inputs(tx)?.into(),
        outputs: compile_outputs(tx, network)?,
        fee: coercion::expr_into_number(&tx.fees)? as u64,
        certificates: primitives::NonEmptySet::from_vec(compile_certs(tx, network)?),
        mint: compile_mint_block(tx)?,
        reference_inputs: primitives::NonEmptySet::from_vec(compile_reference_inputs(tx)?),
        network_id: Some(network),
        ttl: until,
        validity_interval_start: since,
        withdrawals: compile_withdrawals(tx, network)?,
        auxiliary_data_hash: None,
        script_data_hash: None,
        collateral: primitives::NonEmptySet::from_vec(compile_collateral(tx)?),
        required_signers: compile_required_signers(tx)?,
        collateral_return: None,
        total_collateral: None,
        voting_procedures: None,
        proposal_procedures: None,
        treasury_value: None,
        donation: compile_donation(tx)?,
    };

    Ok(out)
}

fn compile_auxiliary_data(tx: &tir::Tx) -> Result<Option<primitives::AuxiliaryData>, Error> {
    let metadata_kv = tx
        .clone()
        .metadata
        .into_iter()
        .map(|x| {
            let key = expr_into_number(&x.key)? as u64;
            let value = expr_into_metadatum(&x.value)?;
            Ok((key, value))
        })
        .collect::<Result<Vec<_>, _>>();

    match metadata_kv {
        Ok(key_values) => {
            let metadata_tree = pallas::ledger::primitives::alonzo::Metadata::from_iter(key_values);
            if metadata_tree.is_empty() {
                Ok(None)
            } else {
                Ok(Some(primitives::AuxiliaryData::PostAlonzo(
                    pallas::ledger::primitives::alonzo::PostAlonzoAuxiliaryData {
                        metadata: Some(metadata_tree),
                        native_scripts: None,
                        plutus_scripts: None,
                    },
                )))
            }
        }
        Err(err) => Err(err),
    }
}

fn utxo_ref_matches(ref1: &UtxoRef, ref2: &primitives::TransactionInput) -> bool {
    ref1.txid.eq(ref2.transaction_id.as_slice()) && ref1.index == ref2.index as u32
}

fn compile_single_spend_redeemer(
    input_id: &UtxoRef,
    redeemer: &tir::Expression,
    sorted_inputs: &[&primitives::TransactionInput],
) -> Result<primitives::Redeemer, Error> {
    let index = sorted_inputs
        .iter()
        .position(|x| utxo_ref_matches(input_id, x))
        .unwrap();

    let redeemer = primitives::Redeemer {
        tag: primitives::RedeemerTag::Spend,
        index: index as u32,
        ex_units: EXECUTION_UNITS,
        data: redeemer.try_as_data()?,
    };

    Ok(redeemer)
}

fn compile_spend_redeemers(
    tx: &tir::Tx,
    compiled_body: &primitives::TransactionBody,
) -> Result<Vec<primitives::Redeemer>, Error> {
    let mut compiled_inputs = compiled_body.inputs.iter().collect::<Vec<_>>();
    compiled_inputs.sort_by_key(|x| (x.transaction_id, x.index));

    let mut redeemers = Vec::new();

    for input in tx.inputs.iter() {
        let utxo = coercion::expr_into_utxo_refs(&input.utxos)?;
        let utxo = utxo
            .first()
            .ok_or(Error::MissingExpression("missing utxo".to_string()))?;

        if let Some(redeemer) = input.redeemer.as_option() {
            let redeemer =
                compile_single_spend_redeemer(utxo, redeemer, compiled_inputs.as_slice())?;
            redeemers.push(redeemer);
        }
    }

    Ok(redeemers)
}

pub fn mint_redeemer_index(
    compiled_body: &primitives::TransactionBody,
    policy: primitives::ScriptHash,
) -> Result<u32, Error> {
    let mut keys: Vec<_> = compiled_body
        .mint
        .iter()
        .flatten()
        .map(|(p, _)| *p)
        .collect();

    keys.sort();
    keys.dedup();

    if let Some(index) = keys.iter().position(|p| *p == policy) {
        return Ok(index as u32);
    }

    Err(Error::ConsistencyError(
        "missing minting policy".to_string(),
    ))
}

fn compile_single_mint_redeemer(
    mint: &tir::Mint,
    compiled_body: &primitives::TransactionBody,
) -> Result<Option<primitives::Redeemer>, Error> {
    let Some(red) = mint.redeemer.as_option() else {
        return Ok(None);
    };

    let assets: Vec<tir::AssetExpr> = coercion::expr_into_assets(&mint.amount)?;
    // TODO: This only works with the first redeemer.
    // Are we allowed to include more than one?
    let asset = assets
        .first()
        .ok_or(Error::MissingExpression("missing asset".to_string()))?;
    let policy = coercion::expr_into_bytes(&asset.policy)?;
    let policy = primitives::Hash::from(policy.as_slice());

    let out = primitives::Redeemer {
        tag: primitives::RedeemerTag::Mint,
        index: mint_redeemer_index(compiled_body, policy)?,
        ex_units: EXECUTION_UNITS,
        data: red.try_as_data()?,
    };

    Ok(Some(out))
}

fn compile_mint_redeemers(
    tx: &tir::Tx,
    compiled_body: &primitives::TransactionBody,
) -> Result<Vec<primitives::Redeemer>, Error> {
    let redeemers = tx
        .mints
        .iter()
        .map(|mint| compile_single_mint_redeemer(mint, compiled_body))
        .filter_map(|x| x.transpose())
        .collect::<Result<Vec<_>, _>>()?;

    Ok(redeemers)
}

fn compile_burn_redeemers(
    tx: &tir::Tx,
    compiled_body: &primitives::TransactionBody,
) -> Result<Vec<primitives::Redeemer>, Error> {
    let redeemers = tx
        .burns
        .iter()
        .map(|mint| compile_single_mint_redeemer(mint, compiled_body))
        .filter_map(|x| x.transpose())
        .collect::<Result<Vec<_>, _>>()?;

    Ok(redeemers)
}

fn withdrawal_redeemer_index(
    compiled_body: &primitives::TransactionBody,
    adhoc: &tir::AdHocDirective,
    network: Network,
) -> Result<u32, Error> {
    let mut keys = compiled_body
        .withdrawals
        .iter()
        .flatten()
        .map(|(cred, _)| cred.as_slice())
        .collect::<Vec<_>>();

    keys.sort();
    keys.dedup();

    let credential = adhoc
        .data
        .get("credential")
        .ok_or(Error::MissingExpression(
            "withdrawal credential".to_string(),
        ))?;
    let credential = coercion::expr_into_reward_account(credential, network)?;

    if let Some(index) = keys.iter().position(|p| *p == credential.as_slice()) {
        return Ok(index as u32);
    }

    Err(Error::ConsistencyError("missing withdrawal".to_string()))
}

fn compile_single_withdrawal_redeemer(
    adhoc: &tir::AdHocDirective,
    compiled_body: &primitives::TransactionBody,
    network: Network,
) -> Result<Option<primitives::Redeemer>, Error> {
    let redeemer = adhoc
        .data
        .get("redeemer")
        .ok_or(Error::MissingExpression("missing redeemer".to_string()))?;

    match redeemer {
        tir::Expression::None => Ok(None),
        _ => Ok(Some(primitives::Redeemer {
            tag: primitives::RedeemerTag::Reward,
            index: withdrawal_redeemer_index(compiled_body, adhoc, network)?,
            ex_units: EXECUTION_UNITS,
            data: redeemer.try_as_data()?,
        })),
    }
}

fn compile_withdrawal_redeemers(
    tx: &tir::Tx,
    compiled_body: &primitives::TransactionBody,
    network: Network,
) -> Result<Vec<primitives::Redeemer>, Error> {
    let redeemers = tx
        .adhoc
        .iter()
        .filter(|x| x.name.as_str() == "withdraw")
        .map(|adhoc| compile_single_withdrawal_redeemer(adhoc, compiled_body, network))
        .filter_map(|x| x.transpose())
        .collect::<Result<Vec<_>, _>>()?;

    Ok(redeemers)
}

fn compile_redeemers(
    tx: &tir::Tx,
    compiled_body: &primitives::TransactionBody,
    network: Network,
) -> Result<Option<Redeemers>, Error> {
    let spend_redeemers = compile_spend_redeemers(tx, compiled_body)?;
    let mint_redeemers = compile_mint_redeemers(tx, compiled_body)?;
    let burn_redeemers = compile_burn_redeemers(tx, compiled_body)?;
    let withdrawal_redeemers = compile_withdrawal_redeemers(tx, compiled_body, network)?;

    // TODO: chain other redeemers
    let all: Vec<_> = spend_redeemers
        .into_iter()
        .chain(mint_redeemers)
        .chain(burn_redeemers)
        .chain(withdrawal_redeemers)
        .collect();

    let mut map = BTreeMap::new();

    for redeemer in all {
        let key = RedeemersKey {
            tag: redeemer.tag,
            index: redeemer.index,
        };

        let value = RedeemersValue {
            ex_units: redeemer.ex_units,
            data: redeemer.data,
        };

        map.insert(key, value);
    }

    if map.is_empty() {
        Ok(None)
    } else {
        Ok(Some(primitives::Redeemers::Map(map)))
    }
}

/// Crawls the transaction body to find all the required scripts.
fn infer_required_scripts(
    compiled_body: &primitives::TransactionBody,
) -> HashSet<primitives::Hash<28>> {
    let mint_policies = compiled_body
        .mint
        .iter()
        .flatten()
        .map(|(p, _)| *p)
        .collect::<Vec<_>>();

    // TODO: evaluate inputs searching for script addresses

    // TODO: evaluate withdrawals searching for script addresses

    // TODO: other sources for scripts

    let all_scripts = HashSet::from_iter(mint_policies);

    // TODO: remove if script is already present via reference inputs

    all_scripts
}

fn find_script_for_hash(tx: &tir::Tx, hash: primitives::Hash<28>) -> Option<tir::Expression> {
    // TODO: we need to track script symbols as part of the IR
    todo!()
}

fn compile_adhoc_plutus_witness<const V: usize>(tx: &tir::Tx) -> Vec<PlutusScript<V>> {
    let out: Vec<_> = tx
        .adhoc
        .iter()
        .filter(|x| x.name.as_str() == "plutus_witness")
        .filter(|adhoc| {
            adhoc
                .data
                .get("version")
                .map(|x| coercion::expr_into_number(x).unwrap_or(0))
                .map(|x| x == V as i128)
                .unwrap_or(false)
        })
        .filter_map(|adhoc| {
            adhoc
                .data
                .get("script")
                .map(coercion::expr_into_bytes)
                .transpose()
                .unwrap_or(None)
        })
        .map(PlutusScript::<V>)
        .collect();

    out
}

pub type NativeWitness = KeepRaw<'static, primitives::NativeScript>;

fn compile_adhoc_native_witness(tx: &tir::Tx) -> Result<Vec<NativeWitness>, Error> {
    tx.adhoc
        .iter()
        .filter(|x| x.name.as_str() == "native_witness")
        .filter_map(|adhoc| {
            adhoc
                .data
                .get("script")
                .map(coercion::expr_into_bytes)
                .transpose()
                .unwrap_or(None)
        })
        .map(|script_bytes| {
            pallas::codec::minicbor::decode::<primitives::NativeScript>(&script_bytes)
                .map(KeepRaw::from)
                .map_err(|_| Error::FormatError("error decoding native script cbor".to_string()))
        })
        .collect::<Result<Vec<_>, _>>()
}

fn compile_witness_set(
    tx: &tir::Tx,
    compiled_body: &primitives::TransactionBody,
    network: Network,
) -> Result<primitives::WitnessSet<'static>, Error> {
    // TODO: the long-term goal is for the compiler to infer the required scripts by
    // inspecting the built transaction. We can do that right now, but we're block
    // trying to get the actual bytes for the scripts. The Tx IR that reaches
    // the compiler erases script data since it's not part of the Tx body.

    /*
    let required_scripts = infer_required_scripts(compiled_body);
    let scripts = required_scripts.iter().find_map(|hash| find_script_for_hash(tx, *hash));
     */

    let witness_set = primitives::WitnessSet {
        redeemer: compile_redeemers(tx, compiled_body, network)?.map(|x| x.into()),
        vkeywitness: None,
        native_script: NonEmptySet::from_vec(compile_adhoc_native_witness(tx)?),
        bootstrap_witness: None,
        plutus_data: None,
        plutus_v1_script: NonEmptySet::from_vec(compile_adhoc_plutus_witness::<1>(tx)),
        plutus_v2_script: NonEmptySet::from_vec(compile_adhoc_plutus_witness::<2>(tx)),
        plutus_v3_script: NonEmptySet::from_vec(compile_adhoc_plutus_witness::<3>(tx)),
    };

    Ok(witness_set)
}

#[derive(Clone, Copy)]
enum ScriptVersion {
    V1,
    V2,
    V3,
}

impl ScriptVersion {
    fn to_plutus_version(self) -> Option<PlutusVersion> {
        match self {
            ScriptVersion::V1 => Some(0),
            ScriptVersion::V2 => Some(1),
            ScriptVersion::V3 => Some(2),
        }
    }
}

fn add_script_ref_from_expr(
    hash_to_version: &mut HashMap<primitives::Hash<28>, ScriptVersion>,
    script_expr: &tir::Expression,
) {
    let bytes = match coercion::expr_into_bytes(script_expr) {
        Ok(b) => b,
        Err(_) => return,
    };
    let script_ref = match minicbor::decode::<primitives::ScriptRef>(bytes.as_slice()) {
        Ok(s) => s,
        Err(_) => return,
    };
    match &script_ref {
        primitives::ScriptRef::PlutusV1Script(s) => {
            hash_to_version.insert(s.compute_hash(), ScriptVersion::V1);
        }
        primitives::ScriptRef::PlutusV2Script(s) => {
            hash_to_version.insert(s.compute_hash(), ScriptVersion::V2);
        }
        primitives::ScriptRef::PlutusV3Script(s) => {
            hash_to_version.insert(s.compute_hash(), ScriptVersion::V3);
        }
        primitives::ScriptRef::NativeScript(_) => {}
    }
}

// Scripts that could be used by the transaction
fn build_script_hash_to_version(
    witness_set: &primitives::WitnessSet,
    tx_body: &primitives::TransactionBody,
    auxiliary_data: Option<&primitives::AuxiliaryData>,
    tx: &tir::Tx,
) -> HashMap<primitives::Hash<28>, ScriptVersion> {
    let mut hash_to_version = HashMap::new();

    // Get scripts available in witnesses
    if let Some(scripts) = witness_set.plutus_v1_script.as_ref() {
        for script in scripts.iter() {
            hash_to_version.insert(script.compute_hash(), ScriptVersion::V1);
        }
    }
    if let Some(scripts) = witness_set.plutus_v2_script.as_ref() {
        for script in scripts.iter() {
            hash_to_version.insert(script.compute_hash(), ScriptVersion::V2);
        }
    }
    if let Some(scripts) = witness_set.plutus_v3_script.as_ref() {
        for script in scripts.iter() {
            hash_to_version.insert(script.compute_hash(), ScriptVersion::V3);
        }
    }

    // Get scripts available in outputs
    for output in tx_body.outputs.iter() {
        let script_ref = match output {
            primitives::TransactionOutput::PostAlonzo(po) => po.script_ref.as_ref(),
            _ => None,
        };
        let Some(cbor_wrap) = script_ref else {
            continue;
        };
        let script_ref = &cbor_wrap.0;
        match script_ref {
            primitives::ScriptRef::PlutusV1Script(s) => {
                hash_to_version.insert(s.compute_hash(), ScriptVersion::V1);
            }
            primitives::ScriptRef::PlutusV2Script(s) => {
                hash_to_version.insert(s.compute_hash(), ScriptVersion::V2);
            }
            primitives::ScriptRef::PlutusV3Script(s) => {
                hash_to_version.insert(s.compute_hash(), ScriptVersion::V3);
            }
            primitives::ScriptRef::NativeScript(_) => {}
        }
    }

    // Get scripts available in auxiliary data (obscure way to include a script in a transaction
    // but possible)
    if let Some(primitives::AuxiliaryData::PostAlonzo(aux)) = auxiliary_data {
        if let Some(scripts) = &aux.plutus_scripts {
            for script in scripts.iter() {
                hash_to_version.insert(script.compute_hash(), ScriptVersion::V1);
            }
        }
    }

    // Get scripts available in inputs
    for input in tx.inputs.iter() {
        if let Some(set) = input.utxos.as_utxo_set() {
            for utxo in set.iter() {
                if let Some(script_expr) = utxo.script.as_ref() {
                    add_script_ref_from_expr(&mut hash_to_version, script_expr);
                }
            }
        }
    }

    // Get scripts available in reference inputs
    for expr in tx.references.iter() {
        if let Some(set) = expr.as_utxo_set() {
            for utxo in set.iter() {
                if let Some(script_expr) = utxo.script.as_ref() {
                    add_script_ref_from_expr(&mut hash_to_version, script_expr);
                }
            }
        }
    }

    hash_to_version
}

/// Collects the set of Plutus versions that are actually used by redeemers in the witness set.
fn infer_plutus_versions_from_redeemers(
    tx: &tir::Tx,
    tx_body: &primitives::TransactionBody,
    witness_set: &primitives::WitnessSet,
    hash_to_version: &HashMap<primitives::Hash<28>, ScriptVersion>,
) -> Result<HashSet<PlutusVersion>, Error> {
    let mut inferred = HashSet::new();

    let redeemer_keys: Vec<primitives::RedeemersKey> = match witness_set.redeemer.as_ref() {
        Some(raw) => match &**raw {
            primitives::Redeemers::Map(m) => m.keys().cloned().collect(),
            primitives::Redeemers::List(list) => list
                .iter()
                .map(|r| primitives::RedeemersKey {
                    tag: r.tag,
                    index: r.index,
                })
                .collect(),
        },
        None => return Ok(inferred),
    };

    let mut sorted_inputs: Vec<_> = tx_body.inputs.iter().collect();
    sorted_inputs.sort_by_key(|x| (x.transaction_id.as_slice(), x.index));

    let mut mint_policies: Vec<primitives::Hash<28>> =
        tx_body.mint.iter().flatten().map(|(p, _)| *p).collect();
    mint_policies.sort();
    mint_policies.dedup();

    let withdrawal_keys: Vec<_> = tx_body
        .withdrawals
        .iter()
        .flatten()
        .map(|(k, _)| k.as_slice())
        .collect();

    let certs: Vec<_> = tx_body
        .certificates
        .as_ref()
        .map(|c| c.iter().collect::<Vec<_>>())
        .unwrap_or_default();

    for key in &redeemer_keys {
        let version_opt = match key.tag {
            primitives::RedeemerTag::Spend => {
                let index = key.index as usize;
                let input_ref = sorted_inputs.get(index).ok_or_else(|| {
                    Error::ConsistencyError(format!(
                        "spend redeemer at index {}: no transaction input at that index",
                        index
                    ))
                })?;
                let tir_input = tx.inputs.iter().find(|inp| {
                    coercion::expr_into_utxo_refs(&inp.utxos)
                        .ok()
                        .and_then(|refs| refs.first().cloned())
                        .is_some_and(|r| utxo_ref_matches(&r, input_ref))
                });
                let utxo = tir_input
                    .and_then(|inp| inp.utxos.as_utxo_set())
                    .and_then(|set| set.iter().next())
                    .ok_or_else(|| {
                        Error::ConsistencyError(format!(
                            "cannot resolve script for spend redeemer at index {} (input or UTXO not present in TIR)",
                            index
                        ))
                    })?;
                let address = coercion::bytes_into_address(&utxo.address).map_err(|_| {
                    Error::ConsistencyError(format!(
                        "spend redeemer at index {}: invalid address in UTXO",
                        index
                    ))
                })?;
                let script_hash = match &address {
                    pallas::ledger::addresses::Address::Shelley(s) => match s.payment() {
                        pallas::ledger::addresses::ShelleyPaymentPart::Script(h) => *h,
                        pallas::ledger::addresses::ShelleyPaymentPart::Key(_) => {
                            return Err(Error::ConsistencyError(format!(
                                "spend redeemer at index {}: input is key-locked, not a script address",
                                index
                            )))
                        }
                    },
                    _ => {
                        return Err(Error::ConsistencyError(format!(
                            "spend redeemer at index {}: expected Shelley address",
                            index
                        )))
                    }
                };
                let version = hash_to_version
                    .get(&script_hash)
                    .and_then(|v| v.to_plutus_version());
                if version.is_none() {
                    return Err(Error::ConsistencyError(format!(
                        "script hash {} referenced by spend redeemer at index {} not found (script must appear in witness set, outputs, auxiliary data, or consumed/reference inputs)",
                        hex::encode(script_hash.as_slice()),
                        index
                    )));
                }
                version
            }
            primitives::RedeemerTag::Mint => {
                let index = key.index as usize;
                let policy = mint_policies.get(index).ok_or_else(|| {
                    Error::ConsistencyError(format!(
                        "mint redeemer at index {} does not match any mint policy",
                        index
                    ))
                })?;
                let version = hash_to_version
                    .get(policy)
                    .and_then(|v| v.to_plutus_version());
                if version.is_none() {
                    return Err(Error::ConsistencyError(format!(
                        "script hash {} (mint policy) referenced by mint redeemer at index {} not found",
                        hex::encode(policy.as_slice()),
                        index
                    )));
                }
                version
            }
            primitives::RedeemerTag::Reward => {
                let index = key.index as usize;
                let cred_slice = withdrawal_keys.get(index).ok_or_else(|| {
                    Error::ConsistencyError(format!(
                        "reward redeemer at index {} does not match any withdrawal",
                        index
                    ))
                })?;
                if cred_slice.is_empty() {
                    return Err(Error::ConsistencyError(format!(
                        "reward redeemer at index {}: empty credential",
                        index
                    )));
                }
                let script_hash = primitives::Hash::from(*cred_slice);
                let version = hash_to_version
                    .get(&script_hash)
                    .and_then(|v| v.to_plutus_version());
                if version.is_none() {
                    return Err(Error::ConsistencyError(format!(
                        "script hash {} referenced by reward redeemer at index {} not found",
                        hex::encode(script_hash.as_slice()),
                        index
                    )));
                }
                version
            }
            primitives::RedeemerTag::Cert => {
                let index = key.index as usize;
                let cert = certs.get(index).ok_or_else(|| {
                    Error::ConsistencyError(format!(
                        "cert redeemer at index {} does not match any certificate",
                        index
                    ))
                })?;
                let cred = match cert {
                    primitives::Certificate::StakeRegistration(c)
                    | primitives::Certificate::StakeDeregistration(c)
                    | primitives::Certificate::StakeDelegation(c, _)
                    | primitives::Certificate::Reg(c, _)
                    | primitives::Certificate::UnReg(c, _)
                    | primitives::Certificate::VoteDeleg(c, _)
                    | primitives::Certificate::StakeVoteDeleg(c, _, _)
                    | primitives::Certificate::StakeRegDeleg(c, _, _)
                    | primitives::Certificate::VoteRegDeleg(c, _, _)
                    | primitives::Certificate::StakeVoteRegDeleg(c, _, _, _)
                    | primitives::Certificate::AuthCommitteeHot(c, _)
                    | primitives::Certificate::ResignCommitteeCold(c, _)
                    | primitives::Certificate::RegDRepCert(c, _, _)
                    | primitives::Certificate::UnRegDRepCert(c, _)
                    | primitives::Certificate::UpdateDRepCert(c, _) => c,
                    _ => {
                        return Err(Error::ConsistencyError(format!(
                            "cert redeemer at index {}: certificate has no stake credential",
                            index
                        )))
                    }
                };
                let script_hash = match cred {
                    primitives::StakeCredential::ScriptHash(h) => *h,
                    primitives::StakeCredential::AddrKeyhash(_) => {
                        return Err(Error::ConsistencyError(format!(
                            "cert redeemer at index {}: certificate is key-based, not a script",
                            index
                        )))
                    }
                };
                let version = hash_to_version
                    .get(&script_hash)
                    .and_then(|v| v.to_plutus_version());
                if version.is_none() {
                    return Err(Error::ConsistencyError(format!(
                        "script hash {} referenced by cert redeemer at index {} not found",
                        hex::encode(script_hash.as_slice()),
                        index
                    )));
                }
                version
            }
            primitives::RedeemerTag::Vote | primitives::RedeemerTag::Propose => Some(2),
        };

        if let Some(v) = version_opt {
            inferred.insert(v);
        }
    }

    Ok(inferred)
}

fn compute_script_data_hash(
    tx: &tir::Tx,
    tx_body: &primitives::TransactionBody,
    auxiliary_data: Option<&primitives::AuxiliaryData>,
    witness_set: &primitives::WitnessSet,
    pparams: &PParams,
) -> Result<Option<primitives::Hash<32>>, Error> {
    let hash_to_version = build_script_hash_to_version(witness_set, tx_body, auxiliary_data, tx);
    let inferred_versions =
        infer_plutus_versions_from_redeemers(tx, tx_body, witness_set, &hash_to_version)?;

    if inferred_versions.is_empty() {
        return Ok(None);
    }

    let language_views_entries: Vec<(PlutusVersion, _)> = inferred_versions
        .into_iter()
        .filter_map(|version| {
            pparams
                .cost_models
                .get(&version)
                .map(|cost_model| (version, cost_model.clone()))
        })
        .collect();

    if language_views_entries.is_empty() {
        return Ok(None);
    }

    let language_views = primitives::LanguageViews::from_iter(language_views_entries);

    let data = primitives::ScriptData::build_for(witness_set, &Some(language_views));

    Ok(data.map(|x| x.hash()))
}

pub fn entry_point(tx: &tir::Tx, pparams: &PParams) -> Result<primitives::Tx<'static>, Error> {
    let mut transaction_body = compile_tx_body(tx, pparams.network)?;
    let transaction_witness_set = compile_witness_set(tx, &transaction_body, pparams.network)?;
    let auxiliary_data = compile_auxiliary_data(tx)?;

    transaction_body.auxiliary_data_hash = auxiliary_data.as_ref().map(|x| x.compute_hash());
    transaction_body.script_data_hash = compute_script_data_hash(
        tx,
        &transaction_body,
        auxiliary_data.as_ref(),
        &transaction_witness_set,
        pparams,
    )?;

    Ok(primitives::Tx {
        transaction_body: transaction_body.into(),
        transaction_witness_set: transaction_witness_set.into(),
        auxiliary_data: primitives::Nullable::from(auxiliary_data.map(KeepRaw::from)),
        success: true,
    })
}
