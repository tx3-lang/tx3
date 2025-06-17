use std::collections::BTreeMap;

use pallas::{
    codec::utils::{KeepRaw, MaybeIndefArray},
    ledger::{
        addresses::{Address, ShelleyPaymentPart},
        primitives::{
            conway::{self as primitives, Redeemers},
            TransactionInput,
        },
        traverse::ComputeHash,
    },
};

use tx3_lang::ir;

use crate::coercion::{expr_into_metadatum, expr_into_number};

use super::*;

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

fn compile_struct(ir: &ir::StructExpr) -> Result<primitives::PlutusData, Error> {
    let fields = ir
        .fields
        .iter()
        .map(compile_data_expr)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(plutus_data::constr(ir.constructor as u64, fields))
}

fn compile_data_expr(ir: &ir::Expression) -> Result<primitives::PlutusData, Error> {
    match ir {
        ir::Expression::None => Ok(().as_data()),
        ir::Expression::Bytes(x) => Ok(x.as_data()),
        ir::Expression::Number(x) => Ok(x.as_data()),
        ir::Expression::Bool(x) => Ok(x.as_data()),
        ir::Expression::String(x) => Ok(x.as_str().as_data()),
        ir::Expression::Struct(x) => compile_struct(x),
        ir::Expression::Address(x) => Ok(x.as_data()),
        _ => Err(Error::CoerceError(
            format!("{:?}", ir),
            "DataExpr".to_string(),
        )),
    }
}

fn compile_native_asset_for_output(
    ir: &ir::AssetExpr,
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
    ir: &ir::AssetExpr,
) -> Result<primitives::Multiasset<primitives::NonZeroInt>, Error> {
    let policy = coercion::expr_into_bytes(&ir.policy)?;
    let policy = primitives::Hash::from(policy.as_slice());
    let asset_name = coercion::expr_into_bytes(&ir.asset_name)?;
    let amount = coercion::expr_into_number(&ir.amount)?;
    let amount = primitives::NonZeroInt::try_from(amount as i64).unwrap();

    let asset = asset!(policy, asset_name.clone(), amount);

    Ok(asset)
}

fn compile_ada_value(ir: &ir::AssetExpr) -> Result<primitives::Value, Error> {
    let amount = coercion::expr_into_number(&ir.amount)?;

    Ok(value!(amount as u64))
}

fn compile_value(ir: &ir::AssetExpr) -> Result<primitives::Value, Error> {
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

// calculate min utxo lovelace according to spec
// https://cips.cardano.org/cip/CIP-55

/*
fn eval_minutxo_constructor(&self, ctr: &ir::AssetConstructor) -> Result<primitives::Value, Error> {
    let ratio = self.ledger.get_pparams()?.coins_per_utxo_byte;
    let output = self.eval.find_output(ctr.name.as_str())?;
    let serialized = pallas_codec::minicbor::to_vec(output).unwrap();
    let min_lovelace = (160u64 + serialized.len() as u64) * ratio;

    Ok(value!(min_lovelace as u64))

    todo!()
}
    */

fn compile_output_block(
    ir: &ir::Output,
    network: Network,
) -> Result<primitives::TransactionOutput<'static>, Error> {
    let address = ir
        .address
        .as_ref()
        .map(|x| coercion::expr_into_address(x, network))
        .transpose()?
        .ok_or(Error::MissingAddress)?;

    let asset_list = ir
        .amount
        .iter()
        .map(coercion::expr_into_assets)
        .collect::<Result<Vec<_>, _>>()?;

    let values = asset_list
        .iter()
        .flatten()
        .map(compile_value)
        .collect::<Result<Vec<_>, _>>()?;

    let value = asset_math::aggregate_values(values);

    let datum_option = ir.datum.as_ref().map(compile_data_expr).transpose()?;

    let output = primitives::TransactionOutput::PostAlonzo(
        primitives::PostAlonzoTransactionOutput {
            address: address.to_vec().into(),
            value,
            datum_option: datum_option.map(|x| {
                primitives::DatumOption::Data(pallas::codec::utils::CborWrap(x.into())).into()
            }),
            script_ref: None, // TODO: add script ref
        }
        .into(),
    );

    Ok(output)
}

fn compile_mint_block(tx: &ir::Tx) -> Result<Option<primitives::Mint>, Error> {
    let mint = if !tx.mints.is_empty() {
        let assets = tx
            .mints
            .iter()
            .flat_map(|x| x.amount.as_ref().map(coercion::expr_into_assets))
            .collect::<Result<Vec<_>, _>>()?;
        let assets = assets
            .iter()
            .flatten()
            .map(compile_native_asset_for_mint)
            .collect::<Result<Vec<_>, _>>()?;

        let value = asset_math::aggregate_assets(assets).unwrap();

        Some(value)
    } else {
        None
    };

    Ok(mint)
}

fn compile_inputs(tx: &ir::Tx) -> Result<Vec<primitives::TransactionInput>, Error> {
    let refs = tx
        .inputs
        .iter()
        .flat_map(|x| x.refs.iter())
        .map(|x| primitives::TransactionInput {
            transaction_id: x.txid.as_slice().into(),
            index: x.index as u64,
        })
        .collect();

    Ok(refs)
}

fn compile_outputs(
    tx: &ir::Tx,
    network: Network,
) -> Result<Vec<primitives::TransactionOutput<'static>>, Error> {
    let resolved = tx
        .outputs
        .iter()
        .map(|x| compile_output_block(x, network))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(resolved)
}

fn compile_vote_delegation_certificate(
    x: &ir::AdHocDirective,
) -> Result<primitives::Certificate, Error> {
    let stake = coercion::expr_into_stake_credential(&x.data["stake"])?;
    let drep = coercion::expr_into_bytes(&x.data["drep"])?;
    let drep = primitives::DRep::Key(drep.as_slice().into());

    Ok(primitives::Certificate::VoteDeleg(stake, drep))
}

fn compile_certs(tx: &ir::Tx) -> Result<Vec<primitives::Certificate>, Error> {
    tx.adhoc
        .iter()
        .filter_map(|x| match x.name.as_str() {
            "vote_delegation_certificate" => {
                let cert = compile_vote_delegation_certificate(x);
                Some(cert)
            }
            _ => None,
        })
        .collect::<Result<Vec<_>, _>>()
}

fn compile_reference_inputs(tx: &ir::Tx) -> Result<Vec<primitives::TransactionInput>, Error> {
    let explicit_ref_inputs = tx
        .references
        .iter()
        .flat_map(coercion::expr_into_utxo_refs)
        .flatten()
        .map(|x| primitives::TransactionInput {
            transaction_id: x.txid.as_slice().into(),
            index: x.index as u64,
        });

    let refs = tx
        .inputs
        .iter()
        .filter_map(|x| x.policy.as_ref())
        .filter_map(|x| x.script.as_ref())
        .filter_map(|x| x.as_utxo_ref())
        .flat_map(|x| coercion::expr_into_utxo_refs(&x))
        .flatten()
        .map(|x| primitives::TransactionInput {
            transaction_id: x.txid.as_slice().into(),
            index: x.index as u64,
        })
        .chain(explicit_ref_inputs)
        .collect();

    Ok(refs)
}

fn compile_collateral(tx: &ir::Tx) -> Result<Vec<TransactionInput>, Error> {
    Ok(tx
        .collateral
        .iter()
        .filter_map(|collateral| collateral.query.r#ref.as_ref())
        .flat_map(|expr| coercion::expr_into_utxo_refs(expr))
        .flatten()
        .map(|x| primitives::TransactionInput {
            transaction_id: x.txid.as_slice().into(),
            index: x.index as u64,
        })
        .collect())
}

fn compile_required_signers(tx: &ir::Tx) -> Result<Option<primitives::RequiredSigners>, Error> {
    let mut hashes = Vec::new();
    let Some(signers) = &tx.signers else {
        return Ok(primitives::RequiredSigners::from_vec(hashes));
    };

    for signer in &signers.signers {
        match signer {
            ir::Expression::String(s) => {
                let signer_addr = coercion::string_into_address(s)?;
                let Address::Shelley(addr) = signer_addr else {
                    return Err(Error::CoerceError(
                        format!("{:?}", signer),
                        "Shelley address".to_string(),
                    ));
                };

                let ShelleyPaymentPart::Key(key) = addr.payment() else {
                    return Err(Error::CoerceError(
                        format!("{:?}", signer),
                        "Key payment credential".to_string(),
                    ));
                };

                hashes.push(*key);
            }
            ir::Expression::Bytes(b) => {
                let bytes = primitives::Bytes::from(b.clone());
                hashes.push(primitives::AddrKeyhash::from(bytes.as_slice()));
            }
            _ => {
                return Err(Error::CoerceError(
                    format!("{:?}", signer),
                    "Signer".to_string(),
                ));
            }
        }
    }

    Ok(primitives::RequiredSigners::from_vec(hashes))
}

fn compile_validity(validity: Option<&ir::Validity>) -> Result<(Option<u64>, Option<u64>), Error> {
    let since = validity
        .and_then(|v| v.since.as_ref())
        .map(|expr| coercion::expr_into_number(expr).map(|n| n as u64))
        .transpose()?;

    let until = validity
        .and_then(|v| v.until.as_ref())
        .map(|expr| coercion::expr_into_number(expr).map(|n| n as u64))
        .transpose()?;

    Ok((since, until))
}

fn compile_tx_body(
    tx: &ir::Tx,
    network: Network,
) -> Result<primitives::TransactionBody<'static>, Error> {
    let (since, until) = compile_validity(tx.validity.as_ref())?;

    let out = primitives::TransactionBody {
        inputs: compile_inputs(tx)?.into(),
        outputs: compile_outputs(tx, network)?,
        fee: coercion::expr_into_number(&tx.fees)? as u64,
        certificates: primitives::NonEmptySet::from_vec(compile_certs(tx)?),
        mint: compile_mint_block(tx)?,
        reference_inputs: primitives::NonEmptySet::from_vec(compile_reference_inputs(tx)?),
        network_id: Some(network),
        ttl: until,
        validity_interval_start: since,
        withdrawals: None,
        auxiliary_data_hash: None,
        script_data_hash: None,
        collateral: primitives::NonEmptySet::from_vec(compile_collateral(tx)?),
        required_signers: compile_required_signers(tx)?,
        collateral_return: None,
        total_collateral: None,
        voting_procedures: None,
        proposal_procedures: None,
        treasury_value: None,
        donation: None,
    };

    Ok(out)
}

fn compile_auxiliary_data(tx: &ir::Tx) -> Result<Option<primitives::AuxiliaryData>, Error> {
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

fn utxo_ref_matches(ref1: &tx3_lang::UtxoRef, ref2: &primitives::TransactionInput) -> bool {
    ref1.txid.eq(ref2.transaction_id.as_slice()) && ref1.index == ref2.index as u32
}

fn compile_single_spend_redeemer(
    input_id: &tx3_lang::UtxoRef,
    redeemer: &ir::Expression,
    sorted_inputs: &[&primitives::TransactionInput],
) -> Result<primitives::Redeemer, Error> {
    let index = sorted_inputs
        .iter()
        .position(|x| utxo_ref_matches(input_id, x))
        .unwrap();

    let redeemer = primitives::Redeemer {
        tag: primitives::RedeemerTag::Spend,
        index: index as u32,
        ex_units: primitives::ExUnits {
            mem: 2000000,
            steps: 2000000000,
        },
        data: redeemer.try_as_data()?,
    };

    Ok(redeemer)
}

fn compile_spend_redeemers(
    tx: &ir::Tx,
    compiled_body: &primitives::TransactionBody,
) -> Result<Vec<primitives::Redeemer>, Error> {
    let mut compiled_inputs = compiled_body.inputs.iter().collect::<Vec<_>>();
    compiled_inputs.sort_by_key(|x| (x.transaction_id, x.index));

    let mut redeemers = Vec::new();

    for input in tx.inputs.iter() {
        for ref_ in input.refs.iter() {
            if let Some(redeemer) = &input.redeemer {
                let redeemer =
                    compile_single_spend_redeemer(ref_, redeemer, compiled_inputs.as_slice())?;
                redeemers.push(redeemer);
            }
        }
    }

    Ok(redeemers)
}

pub fn mint_redeemer_index(
    compiled_body: &primitives::TransactionBody,
    policy: primitives::ScriptHash,
) -> Result<u32, Error> {
    let mut out: Vec<_> = compiled_body
        .mint
        .iter()
        .flat_map(|x| x.iter())
        .map(|(p, _)| *p)
        .collect();

    out.sort();
    out.dedup();

    if let Some(index) = out.iter().position(|p| *p == policy) {
        return Ok(index as u32);
    }

    Err(Error::MissingMintingPolicy)
}

fn compile_mint_redeemer(
    mint: &ir::Mint,
    compiled_body: &primitives::TransactionBody,
) -> Result<primitives::Redeemer, Error> {
    let red = mint.redeemer.clone().ok_or(Error::MissingRedeemer)?;
    let amount = mint.amount.clone().ok_or(Error::MissingAmount)?;
    let assets: Vec<ir::AssetExpr> = coercion::expr_into_assets(&amount)?;
    // TODO: This only works with the first redeemer.
    // Are we allowed to include more than one?
    let asset = assets.first().ok_or(Error::MissingAsset)?;
    let policy = coercion::expr_into_bytes(&asset.policy)?;
    let policy = primitives::Hash::from(policy.as_slice());

    let out = primitives::Redeemer {
        tag: primitives::RedeemerTag::Mint,
        index: mint_redeemer_index(compiled_body, policy)?,
        ex_units: primitives::ExUnits {
            mem: 10000000,
            steps: 2000000000,
        },
        data: red.try_as_data()?,
    };

    Ok(out)
}

fn compile_mint_redeemers(
    tx: &ir::Tx,
    compiled_body: &primitives::TransactionBody,
) -> Result<Vec<primitives::Redeemer>, Error> {
    let redeemers = tx
        .mints
        .iter()
        .map(|mint| compile_mint_redeemer(mint, compiled_body))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(redeemers)
}

fn compile_redeemers(
    tx: &ir::Tx,
    compiled_body: &primitives::TransactionBody,
) -> Result<Option<Redeemers>, Error> {
    let spend_redeemers = compile_spend_redeemers(tx, compiled_body)?;
    let mint_redeemers = compile_mint_redeemers(tx, compiled_body)?;

    // TODO: chain other redeemers
    let redeemers: Vec<_> = spend_redeemers.into_iter().chain(mint_redeemers).collect();

    if redeemers.is_empty() {
        Ok(None)
    } else {
        Ok(Some(primitives::Redeemers::List(
            MaybeIndefArray::Def(redeemers).to_vec(),
        )))
    }
}

fn compile_witness_set(
    tx: &ir::Tx,
    compiled_body: &primitives::TransactionBody,
) -> Result<primitives::WitnessSet<'static>, Error> {
    let witness_set = primitives::WitnessSet {
        redeemer: compile_redeemers(tx, compiled_body)?.map(|x| x.into()),
        vkeywitness: None,
        native_script: None,
        bootstrap_witness: None,
        plutus_v1_script: None,
        plutus_data: None,
        plutus_v2_script: None,
        plutus_v3_script: None,
    };

    Ok(witness_set)
}

fn infer_plutus_version(_transaction_body: &primitives::TransactionBody) -> PlutusVersion {
    // TODO: infer plutus version from existing scripts
    1
}

fn compute_script_data_hash(
    body: &primitives::TransactionBody,
    witness_set: &primitives::WitnessSet,
    pparams: &PParams,
) -> Option<primitives::Hash<32>> {
    let version = infer_plutus_version(body);

    let cost_model = pparams.cost_models.get(&version).unwrap();

    let language_view = primitives::LanguageView(version, cost_model.clone());

    let data = primitives::ScriptData::build_for(witness_set, language_view);

    data.map(|x| x.hash())
}

pub fn compile_tx(tx: &ir::Tx, pparams: &PParams) -> Result<primitives::Tx<'static>, Error> {
    let mut transaction_body = compile_tx_body(tx, pparams.network)?;
    let transaction_witness_set = compile_witness_set(tx, &transaction_body)?;
    let auxiliary_data = compile_auxiliary_data(tx)?;

    transaction_body.script_data_hash =
        compute_script_data_hash(&transaction_body, &transaction_witness_set, pparams);

    transaction_body.auxiliary_data_hash = auxiliary_data
        .as_ref()
        .map(|x| primitives::Bytes::from(x.compute_hash().to_vec()));

    Ok(primitives::Tx {
        transaction_body: transaction_body.into(),
        transaction_witness_set: transaction_witness_set.into(),
        auxiliary_data: primitives::Nullable::from(auxiliary_data.map(KeepRaw::from)),
        success: true,
    })
}
