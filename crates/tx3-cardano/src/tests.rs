use std::collections::{BTreeMap, HashMap, HashSet};

use tx3_lang::Workspace;
use tx3_tir::compile::{CompiledTx, Compiler as _};
use tx3_tir::model::assets::CanonicalAssets;
use tx3_tir::model::core::{Utxo, UtxoRef};
use tx3_tir::model::v1beta0 as tir;
use tx3_tir::reduce::{self, Apply as _, ArgValue};
use tx3_tir::Node as _;

use super::*;

const COST_MODEL_PLUTUS_V1: [i64; 166] = [
    100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4, 16000, 100,
    16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100, 16000, 100, 94375, 32,
    132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848, 228465, 122, 0, 1,
    1, 1000, 42921, 4, 2, 24548, 29498, 38, 1, 898148, 27279, 1, 51775, 558, 1, 39184, 1000, 60594,
    1, 141895, 32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10, 28999, 74, 1, 28999, 74, 1,
    43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391, 32, 11546, 32,
    85848, 228465, 122, 0, 1, 1, 90434, 519, 0, 1, 74433, 32, 85848, 228465, 122, 0, 1, 1, 85848,
    228465, 122, 0, 1, 1, 270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788,
    420, 1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32, 25933, 32, 24623, 32,
    53384111, 14333, 10,
];

static COST_MODEL_PLUTUS_V2: [i64; 175] = [
    100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4, 16000, 100,
    16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100, 16000, 100, 94375, 32,
    132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848, 228465, 122, 0, 1,
    1, 1000, 42921, 4, 2, 24548, 29498, 38, 1, 898148, 27279, 1, 51775, 558, 1, 39184, 1000, 60594,
    1, 141895, 32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10, 28999, 74, 1, 28999, 74, 1,
    43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391, 32, 11546, 32,
    85848, 228465, 122, 0, 1, 1, 90434, 519, 0, 1, 74433, 32, 85848, 228465, 122, 0, 1, 1, 85848,
    228465, 122, 0, 1, 1, 955506, 213312, 0, 2, 270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4,
    0, 141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32, 25933,
    32, 24623, 32, 43053543, 10, 53384111, 14333, 10, 43574283, 26308, 10,
];

fn test_compiler(config: Option<Config>) -> Compiler {
    let pparams = PParams {
        network: crate::Network::Testnet,
        min_fee_coefficient: 1,
        min_fee_constant: 2,
        coins_per_utxo_byte: 1,
        cost_models: HashMap::from([
            (0, COST_MODEL_PLUTUS_V1.to_vec()),
            (1, COST_MODEL_PLUTUS_V2.to_vec()),
            // using the same cost model for v3 for now, need to lookup a valid one
            (2, COST_MODEL_PLUTUS_V2.to_vec()),
        ]),
    };

    let config = config.unwrap_or(Config { extra_fees: None });

    Compiler::new(
        pparams,
        config,
        ChainPoint {
            slot: 101674141,
            hash: vec![],
            timestamp: 1757611408,
        },
    )
}

fn load_protocol(example_name: &str) -> Workspace {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let code = format!("{manifest_dir}/../../examples/{example_name}.tx3");
    let mut ws = tx3_lang::Workspace::from_file(&code).unwrap();
    ws.lower().unwrap();
    ws
}

fn address_to_bytes(address: &str) -> ArgValue {
    ArgValue::Address(
        pallas::ledger::addresses::Address::from_bech32(address)
            .unwrap()
            .to_vec(),
    )
}

fn wildcard_utxos(datum: Option<tir::Expression>) -> HashSet<Utxo> {
    let tx_hash = hex::decode("267aae354f0d14d82877fa5720f7ddc9b0e3eea3cd2a0757af77db4d975ba81c")
        .unwrap()
        .try_into()
        .unwrap();

    let address = pallas::ledger::addresses::Address::from_bech32("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2").unwrap().to_vec();

    let utxo = Utxo {
        r#ref: UtxoRef {
            txid: tx_hash,
            index: 0,
        },
        address,
        datum: Some(datum.unwrap_or(tir::Expression::None)),
        assets: CanonicalAssets::from_naked_amount(500_000_000),
        script: None,
    };

    HashSet::from([utxo])
}

fn fill_inputs(tx: tir::Tx, utxos: HashSet<Utxo>) -> tir::Tx {
    let mut tx = tx;

    for (name, _) in reduce::find_queries(&tx) {
        let utxos = utxos.clone();
        let args = BTreeMap::from([(name.to_string(), utxos)]);
        tx = reduce::apply_inputs(tx, &args).unwrap();
    }

    tx = reduce::reduce(tx).unwrap();

    tx
}

fn compile_tx_round(
    mut tx: tir::Tx,
    args: &BTreeMap<String, ArgValue>,
    fees: u64,
    compiler: &mut Compiler,
    utxos: HashSet<Utxo>,
) -> CompiledTx {
    tx = reduce::apply_args(tx, args).unwrap();
    tx = reduce::apply_fees(tx, fees).unwrap();
    tx = reduce::reduce(tx).unwrap();
    tx = tx.apply(compiler).unwrap();

    tx = fill_inputs(tx, utxos);

    assert!(tx.is_constant());

    compiler.compile(&AnyTir::V1Beta0(tx)).unwrap()
}

fn test_compile(
    tx: tir::Tx,
    args: &BTreeMap<String, ArgValue>,
    compiler: &mut Compiler,
    utxos: HashSet<Utxo>,
) -> CompiledTx {
    let mut fees = 0;
    let mut rounds = 0;
    let mut tx_eval = None;

    while rounds < 3 {
        tx_eval = Some(compile_tx_round(
            tx.clone(),
            args,
            fees,
            compiler,
            utxos.clone(),
        ));
        fees = tx_eval.as_ref().unwrap().fee;
        rounds += 1;
    }

    tx_eval.unwrap()
}

macro_rules! arg_value {
    ($name:expr, "address", $value:expr) => {
        ($name.to_string(), address_to_bytes($value))
    };
    ($name:expr, "int", $value:expr) => {
        ($name.to_string(), ArgValue::Int($value))
    };
    ($name:expr, "bytes_hex", $value:expr) => {
        (
            $name.to_string(),
            ArgValue::Bytes(hex::decode($value).unwrap()),
        )
    };
    ($name:expr, "bool", $value:expr) => {
        ($name.to_string(), ArgValue::Bool($value))
    };
    ($name:expr, "utxo_ref", $txid:expr, $index:expr) => {
        (
            $name.to_string(),
            ArgValue::UtxoRef(UtxoRef {
                txid: hex::decode($txid).unwrap(),
                index: $index,
            }),
        )
    };
}

#[pollster::test]
async fn smoke_test_transfer() {
    let mut compiler = test_compiler(None);
    let utxos = wildcard_utxos(None);
    let protocol = load_protocol("transfer");

    let tx = protocol.tir("transfer").unwrap().clone();

    let args = BTreeMap::from([
        arg_value!("sender", "address", "addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"),
        arg_value!("receiver", "address", "addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"),
        arg_value!("quantity", "int", 100_000_000),
    ]);

    let tx = test_compile(tx, &args, &mut compiler, utxos);

    println!("{}", hex::encode(tx.payload));
    println!("{}", tx.fee);
}

#[pollster::test]
async fn smoke_test_vesting() {
    let mut compiler = test_compiler(None);
    let utxos = wildcard_utxos(None);
    let protocol = load_protocol("vesting");

    let tx = protocol.tir("lock").unwrap().clone();

    let args = BTreeMap::from([
        arg_value!("owner", "address", "addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"),
        arg_value!("beneficiary", "address", "addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"),
        arg_value!("quantity", "int", 100_000_000),
        arg_value!("until", "int", 1713288000),
    ]);

    let tx = test_compile(tx, &args, &mut compiler, utxos);

    println!("{}", hex::encode(tx.payload));

    assert_eq!(
        hex::encode(tx.hash),
        "405feba6368a73bac826c2114640eaefcd46178fdce2aa0dda4be3b2d3daeb28"
    );
}

#[pollster::test]
async fn smoke_test_vesting_unlock() {
    let mut compiler = test_compiler(None);
    let utxos = wildcard_utxos(None);
    let protocol = load_protocol("vesting");

    let tx = protocol.tir("unlock").unwrap().clone();

    let args = BTreeMap::from([
        arg_value!("beneficiary", "address", "addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"),
        arg_value!("locked_utxo", "utxo_ref", "682d6d95495403b491737b95dae5c1f060498d9efc91a592962134f880398be2", 1),
        arg_value!("timelock_script", "utxo_ref", "682d6d95495403b491737b95dae5c1f060498d9efc91a592962134f880398be2", 0),
    ]);

    let tx = test_compile(tx, &args, &mut compiler, utxos);

    println!("{}", hex::encode(tx.payload));

    assert_eq!(
        hex::encode(tx.hash),
        "a720d363d4cc1afa7be67eb4e0a8ddc5e7effe40316d2a49cc19836b9e04c0d4"
    );
}

#[pollster::test]
async fn faucet_test() {
    let mut compiler = test_compiler(None);
    let utxos = wildcard_utxos(None);
    let protocol = load_protocol("faucet");

    let tx = protocol.tir("claim_with_password").unwrap().clone();
    let args = BTreeMap::from([
        arg_value!("quantity", "int", 1),
        arg_value!("password", "bytes_hex", "abc1"),
        arg_value!("requester", "address", "addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"),
    ]);

    let tx = test_compile(tx, &args, &mut compiler, utxos);

    assert_eq!(
        hex::encode(tx.hash),
        "0c699a172b9166f85f21229e47a7d7e24bc0de62f977fe9796795226293647e4"
    );
}

#[pollster::test]
async fn list_concat_test() {
    let mut compiler = test_compiler(None);
    let datum = Some(tir::Expression::Struct(tir::StructExpr {
        constructor: 0,
        fields: vec![tir::Expression::List(vec![])],
    }));
    let utxos = wildcard_utxos(datum);

    let protocol = load_protocol("list_concat");

    let tx = protocol.tir("concat_list").unwrap().clone();

    let args = BTreeMap::from([
        arg_value!("myparty", "address", "addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"),
    ]);

    let tx = test_compile(tx, &args, &mut compiler, utxos);

    assert_eq!(
        hex::encode(tx.hash),
        "bc70528d1ba6d41c31bbbd064722258de7231271acd0fad8d77a8b5823a82181"
    );
}

#[pollster::test]
async fn input_datum_test() {
    let mut compiler = test_compiler(None);

    let utxos = wildcard_utxos(Some(tir::Expression::Struct(tir::StructExpr {
        constructor: 0,
        fields: vec![
            tir::Expression::Number(1),
            tir::Expression::Bytes(b"abc".to_vec()),
        ],
    })));

    let protocol = load_protocol("input_datum");

    let tx = protocol.tir("increase_counter").unwrap().clone();

    let args = BTreeMap::from([
        arg_value!("myparty", "address", "addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"),
    ]);

    let tx = test_compile(tx, &args, &mut compiler, utxos);

    println!("{}", hex::encode(tx.payload));

    assert_eq!(
        hex::encode(tx.hash),
        "4ee1d6ec3b16a883951df96f4c4242931a154439f07a762d078c8683717c6d4c"
    );
}

#[pollster::test]
async fn env_vars_test() {
    let mut compiler = test_compiler(None);
    let utxos = wildcard_utxos(None);
    let protocol = load_protocol("env_vars");

    let tx = protocol.tir("mint_from_env").unwrap().clone();

    let args = BTreeMap::from([
        arg_value!("minter", "address", "addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"),
        arg_value!("mint_policy", "bytes_hex", "682d6d95495403b491737b95dae5c1f060498d9efc91a592962134f8"),
        arg_value!("mint_script", "utxo_ref", "682d6d95495403b491737b95dae5c1f060498d9efc91a592962134f880398be2", 1),
    ]);

    let tx = test_compile(tx, &args, &mut compiler, utxos);

    assert_eq!(
        hex::encode(tx.hash),
        "90f90b26589228954a9d3aeda823b4b74d296bc153e6427d9706537881967c6a"
    );
}

#[pollster::test]
async fn local_vars_test() {
    let mut compiler = test_compiler(None);
    let utxos = wildcard_utxos(None);
    let protocol = load_protocol("local_vars");

    let tx = protocol.tir("mint_from_local").unwrap().clone();

    let args = BTreeMap::from([
        arg_value!("minter", "address", "addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"),
            arg_value!("mint_policy", "bytes_hex", "682d6d95495403b491737b95dae5c1f060498d9efc91a592962134f8"),
        arg_value!("quantity", "int", 100_000_000),
    ]);

    let tx = test_compile(tx, &args, &mut compiler, utxos);

    assert_eq!(
        hex::encode(tx.hash),
        "cb1978d0177fd1a36c282c60d0318766497cc597a17a3483db1c8ff54a547626"
    );
}

#[pollster::test]
async fn adhoc_plutus_witness_test() {
    let mut compiler = test_compiler(None);
    let utxos = wildcard_utxos(None);
    let protocol = load_protocol("cardano_witness");

    let tx = protocol.tir("mint_from_plutus").unwrap().clone();

    let args = BTreeMap::from([
        arg_value!("minter", "address", "addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"),
        arg_value!("quantity", "int", 100_000_000),
    ]);

    let tx = test_compile(tx, &args, &mut compiler, utxos);

    assert_eq!(
        hex::encode(tx.hash),
        "dc2f396b205fe4e5bee3ecbf1d259e8db295c3a7775c9026958d60c3beec5d08"
    );
}

#[pollster::test]
async fn burn_test() {
    let mut compiler = test_compiler(None);
    let utxos = wildcard_utxos(None);
    let protocol = load_protocol("burn");

    let tx = protocol.tir("burn_stuff").unwrap().clone();

    let args = BTreeMap::from([
        arg_value!("burner", "address", "addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"),
        arg_value!("quantity", "int", 100_000_000),
    ]);

    let tx = test_compile(tx, &args, &mut compiler, utxos);

    println!("{}", hex::encode(tx.payload));

    assert_eq!(
        hex::encode(tx.hash),
        "48b172526c29ac9f2193f8bcb276f5fa48acfa27498dfc0dd0faa8df6299e363"
    );
}

#[pollster::test]
async fn adhoc_native_witness_test() {
    let mut compiler = test_compiler(None);
    let utxos = wildcard_utxos(None);
    let protocol = load_protocol("cardano_witness");

    let tx = protocol.tir("mint_from_native_script").unwrap().clone();

    let args = BTreeMap::from([
        arg_value!("minter", "address", "addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"),
        arg_value!("quantity", "int", 100_000_000),
    ]);

    let tx = test_compile(tx, &args, &mut compiler, utxos);

    println!("{}", hex::encode(&tx.payload));

    // For now, just check that the transaction was created successfully
    // The exact hash will depend on the native script implementation
    assert!(!tx.payload.is_empty());
}

#[pollster::test]
async fn extra_fees_test() {
    let extra_fees = 1_200_000;

    let mut compiler = test_compiler(Some(Config {
        extra_fees: Some(extra_fees),
    }));

    let utxos = wildcard_utxos(None);

    let protocol = load_protocol("transfer");

    let tx = protocol.tir("transfer").unwrap().clone();

    let args = BTreeMap::from([
        arg_value!("sender", "address", "addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"),
        arg_value!("receiver", "address", "addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"),
        arg_value!("quantity", "int", 100_000_000),
    ]);

    let tx = test_compile(tx, &args, &mut compiler, utxos);

    assert!(tx.fee >= extra_fees);
}

#[pollster::test]
async fn extra_fees_zero_test() {
    let mut compiler = test_compiler(Some(Config {
        extra_fees: Some(0),
    }));

    let utxos = wildcard_utxos(None);

    let protocol = load_protocol("transfer");

    let tx = protocol.tir("transfer").unwrap().clone();

    let args = BTreeMap::from([
        arg_value!("sender", "address", "addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"),
        arg_value!("receiver", "address", "addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"),
        arg_value!("quantity", "int", 100_000_000),
    ]);

    let tx = test_compile(tx, &args, &mut compiler, utxos);

    assert!(!tx.payload.is_empty());
    assert!(tx.fee < DEFAULT_EXTRA_FEES);
}

#[pollster::test]
async fn min_utxo_test() {
    let mut compiler = test_compiler(None);
    let utxos = wildcard_utxos(None);
    let protocol = load_protocol("min_utxo");

    let tx = protocol.tir("transfer_min").unwrap().clone();

    let args = BTreeMap::from([
        arg_value!("sender", "address", "addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"),
        arg_value!("receiver", "address", "addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"),
    ]);

    let tx = test_compile(tx, &args, &mut compiler, utxos);

    // 224 is the min amount of ada of the first utxo (why? (64 bytes + 160 fixed
    // byets) * 1 coin_per_utxo_byte)
    assert_eq!(
        hex::encode(tx.hash),
        "5853a69e54df5fe4e98692beca8d7d767575617af87b2e971b7c503172de8cb3"
    );
}

#[pollster::test]
async fn min_utxo_compiler_op_test() {
    let compiler = test_compiler(None);

    let result = compiler.reduce_op(tir::CompilerOp::ComputeMinUtxo(tir::Expression::Number(0)));

    assert!(result.is_ok());
    if let Ok(tir::Expression::Assets(assets)) = result {
        assert!(!assets.is_empty());
    }
}
