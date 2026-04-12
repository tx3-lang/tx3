use std::collections::{HashMap, HashSet};

use serde_json::Value;
use tx3_cardano::{ChainPoint, Compiler, Config, Network, PParams};
use tx3_resolver::{resolve_tx, Error, UtxoPattern, UtxoRef, UtxoSet, UtxoStore};
use tx3_tir::encoding::{self, AnyTir, TirVersion};
use tx3_tir::model::assets::AssetClass;
use tx3_tir::model::core::Utxo;
use tx3_tir::reduce::ArgMap;

// ---------------------------------------------------------------------------
// Helpers — parse compiler config from dump JSON
// ---------------------------------------------------------------------------

fn compiler_from_json(config: &Value) -> Compiler {
    let network = match config["network"]
        .as_str()
        .expect("network should be a string")
    {
        "Testnet" => Network::Testnet,
        "Mainnet" => Network::Mainnet,
        other => panic!("unknown network: {other}"),
    };

    let pparams = PParams {
        network,
        min_fee_coefficient: config["min_fee_coefficient"]
            .as_u64()
            .expect("min_fee_coefficient"),
        min_fee_constant: config["min_fee_constant"]
            .as_u64()
            .expect("min_fee_constant"),
        coins_per_utxo_byte: config["coins_per_utxo_byte"]
            .as_u64()
            .expect("coins_per_utxo_byte"),
        cost_models: config["cost_models"]
            .as_object()
            .expect("cost_models should be an object")
            .iter()
            .map(|(k, v)| {
                let version: u8 = k.parse().expect("cost model key should be a number");
                let model: Vec<i64> = v
                    .as_array()
                    .expect("cost model should be an array")
                    .iter()
                    .map(|n| n.as_i64().expect("cost model value should be i64"))
                    .collect();
                (version, model)
            })
            .collect(),
    };

    let extra_fees = config.get("extra_fees").and_then(|v| v.as_u64());

    let chain_point = &config["chain_point"];
    let cursor = ChainPoint {
        slot: chain_point["slot"].as_u64().expect("slot"),
        hash: chain_point["hash"]
            .as_str()
            .map(|s| hex::decode(s).expect("chain_point hash should be valid hex"))
            .unwrap_or_default(),
        timestamp: chain_point["timestamp"].as_u64().expect("timestamp") as u128,
    };

    Compiler::new(pparams, Config { extra_fees }, cursor)
}

// ---------------------------------------------------------------------------
// Fixture parsing — cherry-pick values from dump JSON
// ---------------------------------------------------------------------------

struct ResolveFixture {
    tir: AnyTir,
    args: ArgMap,
    input_pool: HashMap<UtxoRef, Utxo>,
    compiler_config: Value,
    expected_hash: String,
    expected_fee: u64,
}

fn load_fixture(name: &str) -> ResolveFixture {
    let path = format!("{}/tests/golden/{name}.json", env!("CARGO_MANIFEST_DIR"));
    let contents =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("failed to read {path}: {e}"));
    let value: Value =
        serde_json::from_str(&contents).unwrap_or_else(|e| panic!("failed to parse {path}: {e}"));

    // 1. Decode TIR from hex CBOR
    let tir_obj = &value["original_tir"];
    let tir_bytes = hex::decode(
        tir_obj["content"]
            .as_str()
            .expect("tir content should be hex string"),
    )
    .expect("tir content should be valid hex");
    let version = TirVersion::try_from(
        tir_obj["version"]
            .as_str()
            .expect("tir version should be a string"),
    )
    .expect("tir version should be valid");
    let tir = encoding::from_bytes(&tir_bytes, version).expect("tir should decode from CBOR");

    // 2. Parse args using TIR param types + interop
    let params = tx3_tir::reduce::find_params(&tir);
    let mut args = ArgMap::new();
    if let Some(args_obj) = value["args"].as_object() {
        for (key, val) in args_obj {
            if let Some(ty) = params.get(key) {
                let arg = tx3_resolver::interop::from_json(val.clone(), ty)
                    .unwrap_or_else(|e| panic!("failed to parse arg '{key}': {e}"));
                args.insert(key.clone(), arg);
            }
        }
    }

    // 3. Parse input_pool UTxOs
    let mut input_pool = HashMap::new();
    if let Some(pool_obj) = value["input_pool"].as_object() {
        for (_, utxo_val) in pool_obj {
            let utxo = tx3_resolver::interop::utxo_from_json(utxo_val)
                .unwrap_or_else(|e| panic!("failed to parse utxo: {e}"));
            input_pool.insert(utxo.r#ref.clone(), utxo);
        }
    }

    // 4. Parse compiler config
    let compiler_config = value["compiler_config"].clone();
    assert!(
        !compiler_config.is_null(),
        "golden fixture '{name}' is missing compiler_config — regenerate the dump"
    );

    // 5. Read expected values directly
    let expected_hash = value["last_eval"]["hash"]
        .as_str()
        .expect("last_eval.hash should be a hex string")
        .to_string();
    let expected_fee = value["last_eval"]["fee"]
        .as_u64()
        .expect("last_eval.fee should be a number");

    ResolveFixture {
        tir,
        args,
        input_pool,
        compiler_config,
        expected_hash,
        expected_fee,
    }
}

/// Lists all golden dump files available in the `tests/golden/` directory.
fn golden_fixtures() -> Vec<String> {
    let dir = format!("{}/tests/golden", env!("CARGO_MANIFEST_DIR"));
    let mut names = Vec::new();

    for entry in std::fs::read_dir(&dir).expect("golden directory should exist") {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) == Some("json") {
            let stem = path.file_stem().unwrap().to_str().unwrap().to_string();
            names.push(stem);
        }
    }

    names.sort();
    names
}

// ---------------------------------------------------------------------------
// DumpStore — a simple UtxoStore backed by the fixture's input_pool
// ---------------------------------------------------------------------------

struct DumpStore {
    pool: HashMap<UtxoRef, Utxo>,
}

impl From<HashMap<UtxoRef, Utxo>> for DumpStore {
    fn from(pool: HashMap<UtxoRef, Utxo>) -> Self {
        Self { pool }
    }
}

impl DumpStore {
    fn matches_pattern(utxo: &Utxo, pattern: &UtxoPattern<'_>) -> bool {
        match pattern {
            UtxoPattern::ByAddress(addr) => utxo.address == *addr,
            UtxoPattern::ByAssetPolicy(policy) => utxo
                .assets
                .iter()
                .any(|(class, _)| matches!(class, AssetClass::Defined(p, _) if p == policy)),
            UtxoPattern::ByAsset(policy, name) => utxo.assets.asset_amount2(policy, name).is_some(),
        }
    }
}

impl UtxoStore for DumpStore {
    async fn narrow_refs(&self, pattern: UtxoPattern<'_>) -> Result<HashSet<UtxoRef>, Error> {
        let refs = self
            .pool
            .iter()
            .filter(|(_, utxo)| Self::matches_pattern(utxo, &pattern))
            .map(|(r, _)| r.clone())
            .collect();
        Ok(refs)
    }

    async fn fetch_utxos(&self, refs: HashSet<UtxoRef>) -> Result<UtxoSet, Error> {
        Ok(refs
            .iter()
            .filter_map(|r| self.pool.get(r).cloned())
            .collect())
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[pollster::test]
async fn golden_resolve_jobs() {
    let fixtures = golden_fixtures();

    if fixtures.is_empty() {
        eprintln!("no golden dump files found in tests/golden/, skipping");
        return;
    }

    for name in &fixtures {
        let fixture = load_fixture(name);
        let store = DumpStore::from(fixture.input_pool);
        let mut compiler = compiler_from_json(&fixture.compiler_config);

        let compiled = resolve_tx(fixture.tir, &fixture.args, &mut compiler, &store, 5)
            .await
            .unwrap_or_else(|e| panic!("resolve_tx failed for '{name}': {e}"));

        assert_eq!(
            hex::encode(&compiled.hash),
            fixture.expected_hash,
            "hash mismatch for '{name}'"
        );

        assert_eq!(
            compiled.fee, fixture.expected_fee,
            "fee mismatch for '{name}'"
        );
    }
}
