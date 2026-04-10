use std::collections::{HashMap, HashSet};

use tx3_cardano::{ChainPoint, Config, Compiler, Network, PParams};
use tx3_resolver::job::ResolveJob;
use tx3_resolver::{resolve_tx, Error, UtxoPattern, UtxoStore, UtxoRef, UtxoSet};
use tx3_tir::model::assets::AssetClass;
use tx3_tir::model::core::Utxo;

// ---------------------------------------------------------------------------
// Cost models (same as tx3-cardano/src/tests.rs)
// ---------------------------------------------------------------------------

const COST_MODEL_PLUTUS_V1: [i64; 166] = [
    100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4, 16000,
    100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100, 16000, 100, 94375,
    32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848, 228465, 122,
    0, 1, 1, 1000, 42921, 4, 2, 24548, 29498, 38, 1, 898148, 27279, 1, 51775, 558, 1, 39184,
    1000, 60594, 1, 141895, 32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10, 28999, 74,
    1, 28999, 74, 1, 43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32, 72362, 32, 7243, 32,
    7391, 32, 11546, 32, 85848, 228465, 122, 0, 1, 1, 90434, 519, 0, 1, 74433, 32, 85848, 228465,
    122, 0, 1, 1, 85848, 228465, 122, 0, 1, 1, 270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4,
    0, 141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32,
    25933, 32, 24623, 32, 53384111, 14333, 10,
];

static COST_MODEL_PLUTUS_V2: [i64; 175] = [
    100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4, 16000,
    100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100, 16000, 100, 94375,
    32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848, 228465, 122,
    0, 1, 1, 1000, 42921, 4, 2, 24548, 29498, 38, 1, 898148, 27279, 1, 51775, 558, 1, 39184,
    1000, 60594, 1, 141895, 32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10, 28999, 74,
    1, 28999, 74, 1, 43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32, 72362, 32, 7243, 32,
    7391, 32, 11546, 32, 85848, 228465, 122, 0, 1, 1, 90434, 519, 0, 1, 74433, 32, 85848, 228465,
    122, 0, 1, 1, 85848, 228465, 122, 0, 1, 1, 955506, 213312, 0, 2, 270652, 22588, 4, 1457325,
    64566, 4, 20467, 1, 4, 0, 141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32, 20142, 32,
    24588, 32, 20744, 32, 25933, 32, 24623, 32, 43053543, 10, 53384111, 14333, 10, 43574283,
    26308, 10,
];

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn test_compiler() -> Compiler {
    let pparams = PParams {
        network: Network::Testnet,
        min_fee_coefficient: 1,
        min_fee_constant: 2,
        coins_per_utxo_byte: 1,
        cost_models: HashMap::from([
            (0, COST_MODEL_PLUTUS_V1.to_vec()),
            (1, COST_MODEL_PLUTUS_V2.to_vec()),
            (2, COST_MODEL_PLUTUS_V2.to_vec()),
        ]),
    };

    Compiler::new(
        pparams,
        Config { extra_fees: None },
        ChainPoint {
            slot: 101674141,
            hash: vec![],
            timestamp: 1757611408,
        },
    )
}

fn load_dump(name: &str) -> ResolveJob {
    let path = format!(
        "{}/tests/golden/{name}.json",
        env!("CARGO_MANIFEST_DIR")
    );
    let contents = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("failed to read golden file {path}: {e}"));
    serde_json::from_str(&contents)
        .unwrap_or_else(|e| panic!("failed to parse golden file {path}: {e}"))
}

/// Lists all golden dump files available in the `tests/golden/` directory.
fn golden_dumps() -> Vec<String> {
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
// DumpStore — a simple UtxoStore backed by the dump's input_pool
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
            UtxoPattern::ByAssetPolicy(policy) => utxo.assets.iter().any(|(class, _)| {
                matches!(class, AssetClass::Defined(p, _) if p == policy)
            }),
            UtxoPattern::ByAsset(policy, name) => {
                utxo.assets.asset_amount2(policy, name).is_some()
            }
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
    let dumps = golden_dumps();

    if dumps.is_empty() {
        eprintln!("no golden dump files found in tests/golden/, skipping");
        return;
    }

    for name in &dumps {
        let dump = load_dump(name);
        let store = DumpStore::from(dump.input_pool.clone().unwrap_or_default());
        let mut compiler = test_compiler();

        let result = resolve_tx(
            dump.original_tir.clone(),
            &dump.args,
            &mut compiler,
            &store,
            5,
        )
        .await;

        let expected = dump
            .last_eval
            .as_ref()
            .expect("golden dump should contain last_eval");

        let compiled = result.unwrap_or_else(|e| {
            panic!("resolve_tx failed for golden dump '{name}': {e}")
        });

        assert_eq!(
            hex::encode(&compiled.hash),
            hex::encode(&expected.hash),
            "hash mismatch for golden dump '{name}'"
        );

        assert_eq!(
            compiled.fee, expected.fee,
            "fee mismatch for golden dump '{name}'"
        );
    }
}
