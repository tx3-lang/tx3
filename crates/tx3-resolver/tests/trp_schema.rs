use std::collections::HashMap;

use jsonschema::{Draft, JSONSchema};
use serde_json::{json, Map, Value};

use tx3_resolver::interop::{BytesEncoding, BytesEnvelope};
use tx3_resolver::trp::spec::*;

fn load_trp_schema() -> Value {
    let mut trp_schema: Value =
        serde_json::from_str(include_str!("../../../specs/v1beta0/trp.json"))
            .expect("trp.json should parse");

    let core_schema: Value = serde_json::from_str(include_str!("../../../specs/v1beta0/core.json"))
        .expect("core.json should parse");

    let core_defs = core_schema
        .get("$defs")
        .and_then(Value::as_object)
        .expect("core.json should define $defs")
        .clone();

    let components = trp_schema
        .get_mut("components")
        .and_then(Value::as_object_mut)
        .expect("trp.json should define components");

    let schemas = components
        .get_mut("schemas")
        .and_then(Value::as_object_mut)
        .expect("trp.json should define components.schemas");

    for (name, schema) in core_defs {
        schemas.entry(name).or_insert(schema);
    }

    rewrite_core_refs(&mut trp_schema);

    trp_schema
}

fn rewrite_core_refs(value: &mut Value) {
    match value {
        Value::Object(map) => {
            if let Some(Value::String(reference)) = map.get_mut("$ref") {
                if let Some(def_name) = reference.strip_prefix("core.json#/$defs/") {
                    *reference = format!("#/components/schemas/{}", def_name);
                }
            }

            for (_, item) in map.iter_mut() {
                rewrite_core_refs(item);
            }
        }
        Value::Array(items) => {
            for item in items.iter_mut() {
                rewrite_core_refs(item);
            }
        }
        _ => {}
    }
}

fn compile_schema(schema_name: &str) -> JSONSchema {
    let trp_schema = load_trp_schema();
    let schema = json!({
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "$ref": format!("#/components/schemas/{}", schema_name),
        "components": trp_schema.get("components").cloned().unwrap_or(Value::Null),
    });

    JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(&schema)
        .expect("schema should compile")
}

fn assert_schema(schema_name: &str, value: Value) {
    let schema = compile_schema(schema_name);

    let result = schema.validate(&value);
    if let Err(errors) = result {
        let details: Vec<String> = errors.map(|error| error.to_string()).collect();
        panic!(
            "schema validation failed for {}: {}",
            schema_name,
            details.join("; ")
        );
    }
}

fn sample_bytes_envelope() -> BytesEnvelope {
    BytesEnvelope {
        content: "deadbeef".to_string(),
        content_type: BytesEncoding::Hex,
    }
}

fn sample_tir_envelope() -> TirEnvelope {
    TirEnvelope {
        content: "deadbeef".to_string(),
        encoding: BytesEncoding::Hex,
        version: "v1alpha3".to_string(),
    }
}

fn sample_env_map() -> EnvMap {
    let mut map = Map::new();
    map.insert("network".to_string(), json!("preview"));
    map
}

fn sample_args_map() -> JsonArgMap {
    let mut map = Map::new();
    map.insert("amount".to_string(), json!(123));
    map
}

#[test]
fn serialize_submit_params_matches_schema() {
    let params = SubmitParams {
        tx: sample_bytes_envelope(),
        witnesses: vec![TxWitness::RawWitness(sample_bytes_envelope())],
    };

    let value = serde_json::to_value(params).expect("serialize SubmitParams");
    assert_schema("SubmitParams", value);
}

#[test]
fn serialize_tx_envelope_matches_schema() {
    let value = serde_json::to_value(TxEnvelope {
        hash: "abcd".to_string(),
        tx: "deadbeef".to_string(),
    })
    .expect("serialize TxEnvelope");

    assert_schema("TxEnvelope", value);
}

#[test]
fn serialize_submit_response_matches_schema() {
    let value = serde_json::to_value(SubmitResponse {
        hash: "abcd".to_string(),
    })
    .expect("serialize SubmitResponse");

    assert_schema("SubmitResponse", value);
}

#[test]
fn serialize_search_space_diagnostic_matches_schema() {
    let value = serde_json::to_value(SearchSpaceDiagnostic {
        by_address_count: Some(1),
        by_asset_class_count: None,
        by_ref_count: Some(0),
        matched: vec!["0xdeadbeef#0".to_string()],
    })
    .expect("serialize SearchSpaceDiagnostic");

    assert_schema("SearchSpaceDiagnostic", value);
}

#[test]
fn serialize_input_query_diagnostic_matches_schema() {
    let mut min_amount = HashMap::new();
    min_amount.insert("lovelace".to_string(), "100".to_string());

    let value = serde_json::to_value(InputQueryDiagnostic {
        address: None,
        collateral: false,
        min_amount,
        refs: vec!["0xdeadbeef#1".to_string()],
        support_many: true,
    })
    .expect("serialize InputQueryDiagnostic");

    assert_schema("InputQueryDiagnostic", value);
}

#[test]
fn serialize_resolve_params_matches_schema() {
    let value = serde_json::to_value(ResolveParams {
        args: sample_args_map(),
        tir: sample_tir_envelope(),
        env: Some(sample_env_map()),
    })
    .expect("serialize ResolveParams");

    assert_schema("ResolveParams", value);
}

#[test]
fn serialize_tx_signature_matches_schema() {
    let value = serde_json::to_value(TxSignature {
        key: sample_bytes_envelope(),
        signature: sample_bytes_envelope(),
        ty: "vkey".to_string(),
    })
    .expect("serialize TxSignature");

    assert_schema("TxSignature", value);
}

#[test]
fn serialize_tx_witness_signature_matches_schema() {
    let value = serde_json::to_value(TxWitness::Signature(TxSignature {
        key: sample_bytes_envelope(),
        signature: sample_bytes_envelope(),
        ty: "vkey".to_string(),
    }))
    .expect("serialize TxWitness::Signature");

    assert_schema("TxWitness", value);
}

#[test]
fn serialize_tx_witness_raw_matches_schema() {
    let value = serde_json::to_value(TxWitness::RawWitness(sample_bytes_envelope()))
        .expect("serialize TxWitness::RawWitness");

    assert_schema("TxWitness", value);
}

#[test]
fn serialize_unsupported_tir_diagnostic_matches_schema() {
    let value = serde_json::to_value(UnsupportedTirDiagnostic {
        expected: "v1alpha3".to_string(),
        provided: "v1alpha2".to_string(),
    })
    .expect("serialize UnsupportedTirDiagnostic");

    assert_schema("UnsupportedTirDiagnostic", value);
}

#[test]
fn serialize_input_not_resolved_diagnostic_matches_schema() {
    let mut min_amount = HashMap::new();
    min_amount.insert("lovelace".to_string(), "100".to_string());

    let value = serde_json::to_value(InputNotResolvedDiagnostic {
        name: "input1".to_string(),
        query: InputQueryDiagnostic {
            address: Some("addr_test1...".to_string()),
            collateral: true,
            min_amount,
            refs: vec![],
            support_many: false,
        },
        search_space: SearchSpaceDiagnostic {
            by_address_count: None,
            by_asset_class_count: None,
            by_ref_count: None,
            matched: vec![],
        },
    })
    .expect("serialize InputNotResolvedDiagnostic");

    assert_schema("InputNotResolvedDiagnostic", value);
}

#[test]
fn serialize_missing_tx_arg_diagnostic_matches_schema() {
    let value = serde_json::to_value(MissingTxArgDiagnostic {
        key: "amount".to_string(),
        ty: "int".to_string(),
    })
    .expect("serialize MissingTxArgDiagnostic");

    assert_schema("MissingTxArgDiagnostic", value);
}

#[test]
fn serialize_tx_script_failure_diagnostic_matches_schema() {
    let value = serde_json::to_value(TxScriptFailureDiagnostic {
        logs: vec!["fail".to_string()],
    })
    .expect("serialize TxScriptFailureDiagnostic");

    assert_schema("TxScriptFailureDiagnostic", value);
}

#[test]
fn serialize_chain_point_matches_schema() {
    let value = serde_json::to_value(ChainPoint {
        slot: 42,
        block_hash: "abcd".to_string(),
    })
    .expect("serialize ChainPoint");

    assert_schema("ChainPoint", value);
}

#[test]
fn serialize_tx_status_matches_schema() {
    let value = serde_json::to_value(TxStatus {
        stage: "pending".to_string(),
        confirmations: 0,
        non_confirmations: 0,
        confirmed_at: None,
    })
    .expect("serialize TxStatus");

    assert_schema("TxStatus", value);
}

#[test]
fn serialize_check_status_response_matches_schema() {
    let mut statuses = HashMap::new();
    statuses.insert(
        "abcd".to_string(),
        TxStatus {
            stage: "confirmed".to_string(),
            confirmations: 10,
            non_confirmations: 0,
            confirmed_at: Some(ChainPoint {
                slot: 99,
                block_hash: "beef".to_string(),
            }),
        },
    );

    let value = serde_json::to_value(CheckStatusResponse { statuses })
        .expect("serialize CheckStatusResponse");

    assert_schema("CheckStatusResponse", value);
}

#[test]
fn serialize_tx_log_matches_schema() {
    let value = serde_json::to_value(TxLog {
        hash: "abcd".to_string(),
        stage: "finalized".to_string(),
        payload: None,
        confirmations: 20,
        non_confirmations: 0,
        confirmed_at: None,
    })
    .expect("serialize TxLog");

    assert_schema("TxLog", value);
}

#[test]
fn serialize_dump_logs_response_matches_schema() {
    let entries = vec![TxLog {
        hash: "abcd".to_string(),
        stage: "confirmed".to_string(),
        payload: Some("deadbeef".to_string()),
        confirmations: 2,
        non_confirmations: 0,
        confirmed_at: Some(ChainPoint {
            slot: 1,
            block_hash: "beef".to_string(),
        }),
    }];

    let value = serde_json::to_value(DumpLogsResponse {
        entries,
        next_cursor: None,
    })
    .expect("serialize DumpLogsResponse");

    assert_schema("DumpLogsResponse", value);
}

#[test]
fn serialize_pending_tx_matches_schema() {
    let value = serde_json::to_value(PendingTx {
        hash: "abcd".to_string(),
        payload: None,
    })
    .expect("serialize PendingTx");

    assert_schema("PendingTx", value);
}

#[test]
fn serialize_peek_pending_response_matches_schema() {
    let value = serde_json::to_value(PeekPendingResponse {
        entries: vec![PendingTx {
            hash: "abcd".to_string(),
            payload: Some("deadbeef".to_string()),
        }],
    })
    .expect("serialize PeekPendingResponse");

    assert_schema("PeekPendingResponse", value);
}

#[test]
fn serialize_inflight_tx_matches_schema() {
    let value = serde_json::to_value(InflightTx {
        hash: "abcd".to_string(),
        stage: "acknowledged".to_string(),
        confirmations: 1,
        non_confirmations: 0,
        confirmed_at: None,
        payload: None,
    })
    .expect("serialize InflightTx");

    assert_schema("InflightTx", value);
}

#[test]
fn serialize_peek_inflight_response_matches_schema() {
    let value = serde_json::to_value(PeekInflightResponse {
        entries: vec![InflightTx {
            hash: "abcd".to_string(),
            stage: "propagated".to_string(),
            confirmations: 0,
            non_confirmations: 0,
            confirmed_at: Some(ChainPoint {
                slot: 10,
                block_hash: "beef".to_string(),
            }),
            payload: Some("deadbeef".to_string()),
        }],
    })
    .expect("serialize PeekInflightResponse");

    assert_schema("PeekInflightResponse", value);
}
