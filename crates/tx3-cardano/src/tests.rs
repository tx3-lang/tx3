use std::collections::{BTreeMap, HashMap, HashSet};

use tx3_lang::{
    applying, backend::Compiler as _, ir::Node as _, ArgValue, Protocol, Utxo, UtxoRef,
};

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

    Compiler::new(pparams, config)
}

fn load_protocol(example_name: &str) -> Protocol {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let code = format!("{manifest_dir}/../../examples/{example_name}.tx3");
    Protocol::from_file(&code).load().unwrap()
}

fn address_to_bytes(address: &str) -> ArgValue {
    ArgValue::Address(
        pallas::ledger::addresses::Address::from_bech32(address)
            .unwrap()
            .to_vec(),
    )
}

fn wildcard_utxos(datum: Option<tx3_lang::ir::Expression>) -> HashSet<Utxo> {
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
        datum: Some(datum.unwrap_or(tx3_lang::ir::Expression::None)),
        assets: tx3_lang::CanonicalAssets::from_naked_amount(500_000_000),
        script: None,
    };

    HashSet::from([utxo])
}

fn fill_inputs(tx: tx3_lang::ir::Tx, utxos: HashSet<Utxo>) -> tx3_lang::ir::Tx {
    let mut tx = tx;

    for (name, _) in applying::find_queries(&tx) {
        let utxos = utxos.clone();
        let args = BTreeMap::from([(name.to_string(), utxos)]);
        tx = tx3_lang::applying::apply_inputs(tx, &args).unwrap();
    }

    tx = applying::reduce(tx).unwrap();

    tx
}

fn compile_tx_round(
    mut tx: tx3_lang::ir::Tx,
    fees: u64,
    compiler: &mut Compiler,
    utxos: HashSet<Utxo>,
) -> TxEval {
    tx = applying::apply_fees(tx, fees).unwrap();
    tx = applying::reduce(tx).unwrap();
    tx = tx.apply(compiler).unwrap();

    tx = fill_inputs(tx, utxos);

    compiler.compile(&tx).unwrap()
}

fn test_compile(tx: tx3_lang::ir::Tx, compiler: &mut Compiler, utxos: HashSet<Utxo>) -> TxEval {
    let mut fees = 0;
    let mut rounds = 0;
    let mut tx_eval = None;

    while rounds < 3 {
        tx_eval = Some(compile_tx_round(tx.clone(), fees, compiler, utxos.clone()));
        fees = tx_eval.as_ref().unwrap().fee;
        rounds += 1;
    }

    tx_eval.unwrap()
}

#[pollster::test]
async fn smoke_test_transfer() {
    let mut compiler = test_compiler(None);
    let utxos = wildcard_utxos(None);
    let protocol = load_protocol("transfer");

    let tx = protocol.new_tx("transfer")
            .unwrap()
            .with_arg("Sender", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"))
            .with_arg("Receiver", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"))
            .with_arg("quantity", ArgValue::Int(100_000_000))
            .apply()
            .unwrap();

    let tx = test_compile(tx.into(), &mut compiler, utxos);

    println!("{}", hex::encode(tx.payload));
    println!("{}", tx.fee);
}

#[pollster::test]
async fn smoke_test_vesting() {
    let mut compiler = test_compiler(None);
    let utxos = wildcard_utxos(None);
    let protocol = load_protocol("vesting");

    let tx = protocol.new_tx("lock")
            .unwrap()
            .with_arg("Owner", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"))
            .with_arg("Beneficiary", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"))
            .with_arg("quantity", ArgValue::Int(100_000_000))
            .with_arg("until", ArgValue::Int(1713288000))
            .apply()
            .unwrap();

    let tx = test_compile(tx.into(), &mut compiler, utxos);

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

    let tx = protocol.new_tx("unlock")
            .unwrap()
            .with_arg("beneficiary", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"))
            .with_arg("locked_utxo", ArgValue::UtxoRef(tx3_lang::UtxoRef {
                txid: hex::decode("682d6d95495403b491737b95dae5c1f060498d9efc91a592962134f880398be2").unwrap(),
                index: 1,
            }))
            .with_arg("timelock_script", ArgValue::UtxoRef(tx3_lang::UtxoRef {
                txid: hex::decode("682d6d95495403b491737b95dae5c1f060498d9efc91a592962134f880398be2").unwrap(),
                index: 0,
            }))
            .apply()
            .unwrap();

    let tx = test_compile(tx.into(), &mut compiler, utxos);

    println!("{}", hex::encode(tx.payload));

    assert_eq!(
        hex::encode(tx.hash),
        "d8feb5dc4336240f98b82d4c3c1039bb1503c98f671bf2e2980f28ddc1aa81fa"
    );
}

#[pollster::test]
async fn faucet_test() {
    let mut compiler = test_compiler(None);
    let utxos = wildcard_utxos(None);
    let protocol = load_protocol("faucet");

    let mut tx = protocol
        .new_tx("claim_with_password")
        .unwrap()
        .apply()
        .unwrap();

    tx.set_arg("quantity", 1.into());
    tx.set_arg("password", hex::decode("abc1").unwrap().into());
    tx.set_arg("requester", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"));

    let tx = tx.apply().unwrap();

    let tx = test_compile(tx.into(), &mut compiler, utxos);

    assert_eq!(
        hex::encode(tx.hash),
        "3d371b0acf61eb979df1b18c3386141e5b0d54017f4e7e0f2065cde9ee79e6c8"
    );
}

#[pollster::test]
async fn list_concat_test() {
    let mut compiler = test_compiler(None);
    let datum = Some(ir::Expression::Struct(
        ir::StructExpr {
            constructor: 0,
            fields: vec![
                ir::Expression::List(vec![]),
            ],
        },
    ));
    let utxos = wildcard_utxos(datum);
    let protocol = load_protocol("list_concat");
    let mut tx = protocol
        .new_tx("concat_list")
        .unwrap()
        .apply()
        .unwrap();

    tx.set_arg("myparty", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"));

    let tx = tx.apply().unwrap();

    let tx = test_compile(tx.into(), &mut compiler, utxos);

    assert_eq!(
        hex::encode(tx.hash),
        "bc70528d1ba6d41c31bbbd064722258de7231271acd0fad8d77a8b5823a82181"
    );

}

#[pollster::test]
async fn input_datum_test() {
    let mut compiler = test_compiler(None);

    let utxos = wildcard_utxos(Some(tx3_lang::ir::Expression::Struct(
        tx3_lang::ir::StructExpr {
            constructor: 0,
            fields: vec![
                tx3_lang::ir::Expression::Number(1),
                tx3_lang::ir::Expression::Bytes(b"abc".to_vec()),
            ],
        },
    )));

    let protocol = load_protocol("input_datum");

    let mut tx = protocol
        .new_tx("increase_counter")
        .unwrap()
        .apply()
        .unwrap();

    tx.set_arg("myparty", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"));

    let tx = tx.apply().unwrap();

    let tx = test_compile(tx.into(), &mut compiler, utxos);

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

    let mut tx = protocol.new_tx("mint_from_env").unwrap().apply().unwrap();

    tx.set_arg("minter", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"));
    tx.set_arg(
        "mint_policy",
        hex::decode("682d6d95495403b491737b95dae5c1f060498d9efc91a592962134f8")
            .unwrap()
            .into(),
    );
    tx.set_arg(
        "mint_script",
        ArgValue::UtxoRef(UtxoRef {
            txid: hex::decode("682d6d95495403b491737b95dae5c1f060498d9efc91a592962134f880398be2")
                .unwrap(),
            index: 1,
        }),
    );

    let tx = tx.apply().unwrap();

    let tx = test_compile(tx.into(), &mut compiler, utxos);

    assert_eq!(
        hex::encode(tx.hash),
        "4d5f8a6535c06d00a46618fd9dfe06bcb910e5c4306d9a5d3b5180aa5eab93de"
    );
}

#[pollster::test]
async fn local_vars_test() {
    let mut compiler = test_compiler(None);
    let utxos = wildcard_utxos(None);
    let protocol = load_protocol("local_vars");

    let mut tx = protocol.new_tx("mint_from_local").unwrap().apply().unwrap();

    tx.set_arg("minter", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"));
    tx.set_arg(
        "mint_policy",
        hex::decode("682d6d95495403b491737b95dae5c1f060498d9efc91a592962134f8")
            .unwrap()
            .into(),
    );
    tx.set_arg("quantity", ArgValue::Int(100_000_000));

    let tx = tx.apply().unwrap();

    let tx = test_compile(tx.into(), &mut compiler, utxos);

    assert_eq!(
        hex::encode(tx.hash),
        "1f46df35ffcdbc4d31d4a1509b24d8d51c227146771d345a34b7c73d9597d551"
    );
}

#[pollster::test]
async fn adhoc_plutus_witness_test() {
    let mut compiler = test_compiler(None);
    let utxos = wildcard_utxos(None);
    let protocol = load_protocol("cardano_witness");

    let mut tx = protocol
        .new_tx("mint_from_plutus")
        .unwrap()
        .apply()
        .unwrap();

    tx.set_arg("minter", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"));
    tx.set_arg("quantity", ArgValue::Int(100_000_000));

    let tx = tx.apply().unwrap();

    let tx = test_compile(tx.into(), &mut compiler, utxos);

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

    let mut tx = protocol.new_tx("burn_stuff").unwrap().apply().unwrap();

    tx.set_arg("burner", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"));
    tx.set_arg("quantity", ArgValue::Int(100_000_000));

    let tx = tx.apply().unwrap();

    let tx = test_compile(tx.into(), &mut compiler, utxos);

    println!("{}", hex::encode(tx.payload));

    assert_eq!(
        hex::encode(tx.hash),
        "71e84fad764e06316f4e7137ab86cb2665bbca803ac8a1d380d006d8ba355c47"
    );
}

#[pollster::test]
async fn adhoc_native_witness_test() {
    let mut compiler = test_compiler(None);
    let utxos = wildcard_utxos(None);
    let protocol = load_protocol("cardano_witness");

    let mut tx = protocol
        .new_tx("mint_from_native_script")
        .unwrap()
        .apply()
        .unwrap();

    tx.set_arg("minter", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"));
    tx.set_arg("quantity", ArgValue::Int(100_000_000));

    let tx = tx.apply().unwrap();

    let tx = test_compile(tx.into(), &mut compiler, utxos);

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

    let tx = protocol.new_tx("transfer")
            .unwrap()
            .with_arg("Sender", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"))
            .with_arg("Receiver", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"))
            .with_arg("quantity", ArgValue::Int(100_000_000))
            .apply()
            .unwrap();

    let tx = test_compile(tx.into(), &mut compiler, utxos);

    assert!(tx.fee >= extra_fees);
}

#[pollster::test]
async fn extra_fees_zero_test() {
    let mut compiler = test_compiler(Some(Config {
        extra_fees: Some(0),
    }));

    let utxos = wildcard_utxos(None);

    let protocol = load_protocol("transfer");

    let tx = protocol.new_tx("transfer")
            .unwrap()
            .with_arg("Sender", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"))
            .with_arg("Receiver", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"))
            .with_arg("quantity", ArgValue::Int(100_000_000))
            .apply()
            .unwrap();

    let tx = test_compile(tx.into(), &mut compiler, utxos);

    assert!(!tx.payload.is_empty());
    assert!(tx.fee < DEFAULT_EXTRA_FEES);
}
