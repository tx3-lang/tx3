//! Computation of the Cardano script data hash (script integrity hash).
//!
//! The set of Plutus languages that feed the hash is read directly from each
//! script's `ScriptRef` tag — the same approach the Haskell ledger uses —
//! rather than inferred, and is collected across both the inline witness
//! scripts and the reference scripts attached to the resolved UTxOs of spending
//! and reference inputs.

use std::collections::BTreeSet;

use pallas::ledger::primitives::conway as primitives;

use tx3_tir::compile::Error;
use tx3_tir::model::v1beta0 as tir;

use crate::{coercion, PParams, PlutusVersion};

/// Maps a `ScriptRef` variant — whose CBOR tag encodes the script kind
/// (native=0, PlutusV1=1, PlutusV2=2, PlutusV3=3) — to the corresponding
/// `LanguageViews` / cost-model key (PlutusV1=0, PlutusV2=1, PlutusV3=2).
/// Native scripts have no language view, so they return `None`.
fn script_ref_language_key(script_ref: &primitives::ScriptRef) -> Option<PlutusVersion> {
    match script_ref {
        primitives::ScriptRef::NativeScript(_) => None,
        primitives::ScriptRef::PlutusV1Script(_) => Some(0),
        primitives::ScriptRef::PlutusV2Script(_) => Some(1),
        primitives::ScriptRef::PlutusV3Script(_) => Some(2),
    }
}

/// Collects the set of Plutus languages (as `LanguageViews` / cost-model keys)
/// actually used by the transaction. The language of each script is read
/// directly from its script-ref tag — the same approach the Haskell ledger
/// uses — rather than inferred. Sources are the inline witness scripts and the
/// reference scripts attached to the resolved UTxOs of spending and reference
/// inputs.
fn collect_used_plutus_versions(
    tx: &tir::Tx,
    witness_set: &primitives::WitnessSet,
) -> Result<BTreeSet<PlutusVersion>, Error> {
    let mut versions = BTreeSet::new();

    if witness_set.plutus_v1_script.is_some() {
        versions.insert(0);
    }
    if witness_set.plutus_v2_script.is_some() {
        versions.insert(1);
    }
    if witness_set.plutus_v3_script.is_some() {
        versions.insert(2);
    }

    // TODO(Option B): restrict reference-script languages to scripts actually
    // executed by filtering on the required-script hashes (see
    // `infer_required_scripts`); today we include every reference script found
    // on the resolved spending/reference UTxOs.
    let utxo_exprs = tx
        .inputs
        .iter()
        .map(|i| &i.utxos)
        .chain(tx.references.iter());

    for expr in utxo_exprs {
        for utxo in coercion::expr_into_utxos(expr)? {
            if let Some(script_expr) = utxo.script.as_ref() {
                let script_ref = coercion::expr_into_script_ref(script_expr)?;
                if let Some(key) = script_ref_language_key(&script_ref) {
                    versions.insert(key);
                }
            }
        }
    }

    Ok(versions)
}

pub(super) fn compute_script_data_hash(
    tx: &tir::Tx,
    witness_set: &primitives::WitnessSet,
    pparams: &PParams,
) -> Result<Option<primitives::Hash<32>>, Error> {
    let mut versions = collect_used_plutus_versions(tx, witness_set)?;

    // Interim fallback: if the transaction has redeemers (so Plutus scripts
    // will execute) but we could not determine any language -- e.g. the
    // executed scripts are reference scripts whose bytes the provider did not
    // populate on the resolved UTxO -- fall back to the latest language
    // (PlutusV3) so the script data hash stays valid for the common V3-only
    // case instead of omitting the language view entirely. This degraded path
    // disappears once providers populate `utxo.script` (see the Dolos/Hydra TRP
    // mappings), after which the language is read directly from the script-ref
    // tag.
    // TODO: remove once all providers populate reference-script bytes.
    if versions.is_empty() && witness_set.redeemer.is_some() {
        versions.insert(2);
    }

    let mut entries = Vec::with_capacity(versions.len());
    for version in &versions {
        let cost_model = pparams.cost_models.get(version).ok_or_else(|| {
            Error::FormatError(format!(
                "missing cost model for plutus language view key {version}"
            ))
        })?;
        entries.push((*version, cost_model.clone()));
    }

    // Keep `None` (never `Some(empty)`) when no plutus scripts are used: an
    // empty language view map would encode incorrectly, and `build_for` only
    // attaches language views when redeemers are present anyway.
    let language_views =
        (!entries.is_empty()).then(|| primitives::LanguageViews::from_iter(entries));

    let data = primitives::ScriptData::build_for(witness_set, &language_views);

    Ok(data.map(|x| x.hash()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    use pallas::codec::{
        minicbor,
        utils::{CborWrap, KeepRaw, NonEmptySet},
    };
    use pallas::ledger::primitives::conway::{NativeScript, ScriptRef};
    use pallas::ledger::primitives::PlutusScript;
    use proptest::prelude::*;

    use tx3_tir::model::assets::CanonicalAssets;
    use tx3_tir::model::core::{Utxo, UtxoRef, UtxoSet};

    use crate::Network;

    /// Minimal protocol params for the hash tests. Only the cost-model map
    /// matters here; it is left empty (the tests either return before touching
    /// it or deliberately exercise the missing-cost-model path).
    fn test_pparams() -> PParams {
        PParams {
            network: Network::Testnet,
            min_fee_coefficient: 0,
            min_fee_constant: 0,
            coins_per_utxo_byte: 0,
            cost_models: HashMap::new(),
        }
    }

    fn empty_tx() -> tir::Tx {
        tir::Tx {
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
        }
    }

    fn empty_witness_set() -> primitives::WitnessSet<'static> {
        primitives::WitnessSet {
            redeemer: None,
            vkeywitness: None,
            native_script: None,
            bootstrap_witness: None,
            plutus_data: None,
            plutus_v1_script: None,
            plutus_v2_script: None,
            plutus_v3_script: None,
        }
    }

    fn utxo_with_script(script: Option<tir::Expression>) -> UtxoSet {
        let utxo = Utxo {
            r#ref: UtxoRef {
                txid: vec![0u8; 32],
                index: 0,
            },
            address: vec![],
            assets: CanonicalAssets::from_naked_amount(1),
            datum: None,
            script,
        };

        [utxo].into()
    }

    fn ref_script_expr(script_ref: &ScriptRef) -> tir::Expression {
        tir::Expression::Bytes(minicbor::to_vec(script_ref).unwrap())
    }

    fn plutus_v2() -> ScriptRef<'static> {
        ScriptRef::PlutusV2Script(PlutusScript::<2>(vec![0x01, 0x02, 0x03].into()))
    }

    fn plutus_v3() -> ScriptRef<'static> {
        ScriptRef::PlutusV3Script(PlutusScript::<3>(vec![0x01, 0x02, 0x03].into()))
    }

    fn native() -> ScriptRef<'static> {
        let script = NativeScript::ScriptPubkey(primitives::Hash::<28>::from([7u8; 28]));
        ScriptRef::NativeScript(KeepRaw::from(script))
    }

    // A reference input carrying a PlutusV2 reference script, with no inline
    // witness scripts, must yield the V2 language key {1} -- explicitly NOT the
    // old PlutusV3 default {2}.
    #[test]
    fn reference_only_plutus_v2() {
        let mut tx = empty_tx();
        tx.references = vec![tir::Expression::UtxoSet(utxo_with_script(Some(
            ref_script_expr(&plutus_v2()),
        )))];

        let versions = collect_used_plutus_versions(&tx, &empty_witness_set()).unwrap();

        assert_eq!(versions, BTreeSet::from([1]));
        assert!(!versions.contains(&2));
    }

    // A native reference script contributes no language view.
    #[test]
    fn native_reference_script_is_ignored() {
        let mut tx = empty_tx();
        tx.references = vec![tir::Expression::UtxoSet(utxo_with_script(Some(
            ref_script_expr(&native()),
        )))];

        let versions = collect_used_plutus_versions(&tx, &empty_witness_set()).unwrap();

        assert!(versions.is_empty());

        // With no plutus scripts and no redeemers there is no script data hash.
        let hash = compute_script_data_hash(&tx, &empty_witness_set(), &test_pparams()).unwrap();
        assert!(hash.is_none());
    }

    // Property: the collected language set equals exactly the set of Plutus
    // language keys of all scripts present -- across witness scripts, spending
    // inputs and reference inputs -- with native scripts excluded and duplicates
    // deduped, regardless of how the scripts are distributed across sources.
    #[derive(Clone, Copy, Debug)]
    enum Lang {
        Native,
        V1,
        V2,
        V3,
    }

    impl Lang {
        fn script_ref(self, seed: u8) -> ScriptRef<'static> {
            match self {
                Lang::Native => ScriptRef::NativeScript(KeepRaw::from(NativeScript::ScriptPubkey(
                    primitives::Hash::<28>::from([seed; 28]),
                ))),
                Lang::V1 => ScriptRef::PlutusV1Script(PlutusScript::<1>(vec![seed].into())),
                Lang::V2 => ScriptRef::PlutusV2Script(PlutusScript::<2>(vec![seed].into())),
                Lang::V3 => ScriptRef::PlutusV3Script(PlutusScript::<3>(vec![seed].into())),
            }
        }

        /// The expected `LanguageViews` key per the spec (native has none).
        fn key(self) -> Option<PlutusVersion> {
            match self {
                Lang::Native => None,
                Lang::V1 => Some(0),
                Lang::V2 => Some(1),
                Lang::V3 => Some(2),
            }
        }
    }

    #[derive(Clone, Copy, Debug)]
    enum Source {
        Witness,
        SpendingInput,
        ReferenceInput,
    }

    fn utxo_set_with_ref_script(seed: u8, script_ref: &ScriptRef) -> UtxoSet {
        let utxo = Utxo {
            // Unique ref per seed so the UtxoSet does not dedup distinct scripts.
            r#ref: UtxoRef {
                txid: vec![seed; 32],
                index: 0,
            },
            address: vec![],
            assets: CanonicalAssets::from_naked_amount(1),
            datum: None,
            script: Some(ref_script_expr(script_ref)),
        };

        [utxo].into()
    }

    fn build_tx(placements: &[(Source, Lang)]) -> (tir::Tx, primitives::WitnessSet<'static>) {
        let mut tx = empty_tx();
        let mut witness_set = empty_witness_set();

        let mut native_ws = vec![];
        let mut v1_ws = vec![];
        let mut v2_ws = vec![];
        let mut v3_ws = vec![];

        for (i, (source, lang)) in placements.iter().enumerate() {
            let seed = i as u8;
            match source {
                Source::Witness => match lang {
                    Lang::Native => native_ws.push(KeepRaw::from(NativeScript::ScriptPubkey(
                        primitives::Hash::<28>::from([seed; 28]),
                    ))),
                    Lang::V1 => v1_ws.push(PlutusScript::<1>(vec![seed].into())),
                    Lang::V2 => v2_ws.push(PlutusScript::<2>(vec![seed].into())),
                    Lang::V3 => v3_ws.push(PlutusScript::<3>(vec![seed].into())),
                },
                Source::SpendingInput => tx.inputs.push(tir::Input {
                    name: format!("input_{i}"),
                    utxos: tir::Expression::UtxoSet(utxo_set_with_ref_script(
                        seed,
                        &lang.script_ref(seed),
                    )),
                    redeemer: tir::Expression::None,
                }),
                Source::ReferenceInput => {
                    tx.references
                        .push(tir::Expression::UtxoSet(utxo_set_with_ref_script(
                            seed,
                            &lang.script_ref(seed),
                        )))
                }
            }
        }

        witness_set.native_script = NonEmptySet::from_vec(native_ws);
        witness_set.plutus_v1_script = NonEmptySet::from_vec(v1_ws);
        witness_set.plutus_v2_script = NonEmptySet::from_vec(v2_ws);
        witness_set.plutus_v3_script = NonEmptySet::from_vec(v3_ws);

        (tx, witness_set)
    }

    fn lang_strategy() -> impl Strategy<Value = Lang> {
        prop_oneof![
            Just(Lang::Native),
            Just(Lang::V1),
            Just(Lang::V2),
            Just(Lang::V3),
        ]
    }

    fn source_strategy() -> impl Strategy<Value = Source> {
        prop_oneof![
            Just(Source::Witness),
            Just(Source::SpendingInput),
            Just(Source::ReferenceInput),
        ]
    }

    proptest! {
        #[test]
        fn collected_versions_match_present_languages(
            placements in prop::collection::vec((source_strategy(), lang_strategy()), 0..8)
        ) {
            let (tx, witness_set) = build_tx(&placements);

            let expected: BTreeSet<PlutusVersion> =
                placements.iter().filter_map(|(_, lang)| lang.key()).collect();

            let got = collect_used_plutus_versions(&tx, &witness_set).unwrap();

            prop_assert_eq!(got, expected);
        }
    }

    // A used language with no configured cost model surfaces an error rather
    // than panicking (the old `.unwrap()`).
    #[test]
    fn missing_cost_model_errors() {
        let mut tx = empty_tx();
        tx.references = vec![tir::Expression::UtxoSet(utxo_with_script(Some(
            ref_script_expr(&plutus_v3()),
        )))];

        // `test_pparams` has an empty cost model map, so key 2 is missing.
        let result = compute_script_data_hash(&tx, &empty_witness_set(), &test_pparams());
        assert!(matches!(result, Err(Error::FormatError(_))));
    }

    // A reference script double-wrapped in a CborWrap still decodes.
    #[test]
    fn cbor_wrap_fallback() {
        let wrapped = tir::Expression::Bytes(minicbor::to_vec(CborWrap(plutus_v2())).unwrap());

        let script_ref = coercion::expr_into_script_ref(&wrapped).unwrap();
        assert_eq!(script_ref_language_key(&script_ref), Some(1));
    }
}
