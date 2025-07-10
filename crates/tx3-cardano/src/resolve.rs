use pallas::{
    crypto::hash::Hash,
    ledger::{primitives::conway as primitives, traverse::ComputeHash},
};
use tx3_lang::{applying::Apply, ir::InputQuery, UtxoRef};

use crate::{compile::compile_tx, Error, PParams};

const DEFAULT_EXTRA_FEES: u64 = 200_000;

#[derive(Debug, PartialEq)]
pub struct TxEval {
    pub payload: Vec<u8>,
    pub hash: Hash<32>,
    pub fee: u64,
    pub ex_units: u64,
}

#[derive(Debug, Default)]
pub struct ResolveContext {
    pub ignore: Vec<UtxoRef>,
}

#[derive(Debug, Clone)]
pub struct Config {
    pub max_optimize_rounds: usize,
    pub extra_fees: Option<u64>,
}

#[trait_variant::make(Send)]
pub trait Ledger {
    async fn get_pparams(&self) -> Result<PParams, Error>;

    async fn resolve_input(
        &self,
        query: &InputQuery,
        resolve_context: &ResolveContext,
    ) -> Result<tx3_lang::UtxoSet, Error>;
}

fn eval_size_fees(tx: &[u8], pparams: &PParams, extra_fees: Option<u64>) -> Result<u64, Error> {
    Ok(tx.len() as u64 * pparams.min_fee_coefficient
        + pparams.min_fee_constant
        + extra_fees.unwrap_or(DEFAULT_EXTRA_FEES))
}

#[allow(dead_code)]
fn eval_redeemer_fees(_tx: &primitives::Tx, _pparams: &PParams) -> Result<u64, Error> {
    // pallas::ledger::validate::phase_two::evaluate_tx(tx.into(), pparams, utxos,
    // slot_config);
    todo!()
}

async fn resolve_input_queries<L: Ledger>(
    tx: &tx3_lang::ProtoTx,
    ledger: &L,
) -> Result<tx3_lang::ProtoTx, Error> {
    let mut attempt = tx.clone();
    let queries = attempt
        .find_queries()
        .to_owned()
        .into_iter()
        .collect::<Vec<_>>();

    for (name, query) in queries {
        let ignore = if name == "collateral" {
            vec![]
        } else {
            attempt
                .inputs()
                .map(|(_, utxos)| {
                    utxos
                        .iter()
                        .map(|utxo| utxo.r#ref.clone())
                        .collect::<Vec<_>>()
                })
                .flatten()
                .collect::<Vec<_>>()
        };
        let utxos = ledger
            .resolve_input(&query, &ResolveContext { ignore })
            .await?;
        if utxos.is_empty() {
            return Err(Error::InputsNotResolved(
                name.clone(),
                Box::new(query.clone()),
            ));
        }

        attempt.set_input(&name, utxos);
    }
    Ok(attempt)
}

async fn eval_pass<L: Ledger>(
    tx: &tx3_lang::ProtoTx,
    pparams: &PParams,
    ledger: &L,
    last_eval: Option<&TxEval>,
    config: &Config,
) -> Result<Option<TxEval>, Error> {
    let mut attempt = tx.clone();

    let fees = last_eval.as_ref().map(|e| e.fee).unwrap_or(0);
    attempt.set_fees(fees);

    attempt = attempt.apply()?;

    let attempt = resolve_input_queries(&attempt, ledger).await?;

    let attempt = attempt.apply()?;

    if !attempt.as_ref().is_constant() {
        return Err(Error::CantCompileNonConstantTir);
    }

    let tx = compile_tx(attempt.as_ref(), pparams)?;

    let hash = tx.transaction_body.compute_hash();

    let payload = pallas::codec::minicbor::to_vec(&tx).unwrap();

    let size_fees = eval_size_fees(&payload, pparams, config.extra_fees)?;

    //let redeemer_fees = eval_redeemer_fees(tx, pparams)?;

    let eval = Some(TxEval {
        payload,
        hash,
        fee: size_fees, // TODO: add redeemer fees
        ex_units: 0,
    });

    if eval.as_ref() != last_eval {
        return Ok(eval);
    }

    Ok(None)
}

pub async fn resolve_tx<T: Ledger>(
    tx: tx3_lang::ProtoTx,
    ledger: T,
    config: Config,
) -> Result<TxEval, Error> {
    let pparams = ledger.get_pparams().await?;
    let mut last_eval = None;
    let mut rounds = 0;

    // one initial pass to reduce any available params;
    let tx = tx.apply()?;

    while let Some(better) = eval_pass(&tx, &pparams, &ledger, last_eval.as_ref(), &config).await? {
        last_eval = Some(better);

        if rounds > config.max_optimize_rounds {
            return Err(Error::MaxOptimizeRoundsReached);
        }

        rounds += 1;
    }

    Ok(last_eval.unwrap())
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use tx3_lang::{ArgValue, Protocol, UtxoRef};

    use super::*;
    use crate::ledgers::mock::MockLedger;

    impl Default for Config {
        fn default() -> Self {
            Self {
                max_optimize_rounds: 3,
                extra_fees: None,
            }
        }
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

    #[tokio::test]
    async fn smoke_test_transfer() {
        let protocol = load_protocol("transfer");

        let tx = protocol.new_tx("transfer")
            .unwrap()
            .with_arg("Sender", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"))
            .with_arg("Receiver", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"))
            .with_arg("quantity", ArgValue::Int(100_000_000))
            .apply()
            .unwrap();

        let tx = resolve_tx(tx, MockLedger::default(), Config::default())
            .await
            .unwrap();

        println!("{}", hex::encode(tx.payload));
        println!("{}", tx.fee);
    }

    #[tokio::test]
    async fn smoke_test_vesting() {
        let protocol = load_protocol("vesting");

        let tx = protocol.new_tx("lock")
            .unwrap()
            .with_arg("Owner", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"))
            .with_arg("Beneficiary", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"))
            .with_arg("quantity", ArgValue::Int(100_000_000))
            .with_arg("until", ArgValue::Int(1713288000))
            .apply()
            .unwrap();

        let tx = resolve_tx(tx, MockLedger::default(), Config::default())
            .await
            .unwrap();

        dbg!(&tx);

        assert_eq!(
            tx.hash.to_string(),
            "405feba6368a73bac826c2114640eaefcd46178fdce2aa0dda4be3b2d3daeb28"
        );
    }

    #[tokio::test]
    async fn smoke_test_vesting_unlock() {
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

        dbg!(&tx.find_params());
        dbg!(&tx.find_queries());

        let tx = resolve_tx(tx, MockLedger::default(), Config::default())
            .await
            .unwrap();

        dbg!(&tx);

        assert_eq!(
            tx.hash.to_string(),
            "25d3d58935f885f14ff9af81864a4838ef7207c9d49701d94fae21e1e3ae6cbd"
        );
    }

    #[tokio::test]
    async fn faucet_test() {
        let protocol = load_protocol("faucet");

        let mut tx = protocol
            .new_tx("claim_with_password")
            .unwrap()
            .apply()
            .unwrap();

        tx.set_arg("quantity", 1.into());
        tx.set_arg("password", hex::decode("abc1").unwrap().into());
        tx.set_arg("requester", "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x".into());

        dbg!(&tx.find_params());

        let tx = tx.apply().unwrap();

        let tx = resolve_tx(tx, MockLedger::default(), Config::default())
            .await
            .unwrap();

        dbg!(&tx);

        assert_eq!(
            tx.hash.to_string(),
            "fd7c9b7b6a6e4f1989681a08930a0f542b284f23f56c87a2456139a782842fa6"
        );
    }

    #[tokio::test]
    async fn input_datum_test() {
        let protocol = load_protocol("input_datum");

        let mut tx = protocol
            .new_tx("increase_counter")
            .unwrap()
            .apply()
            .unwrap();

        tx.set_arg("myparty", "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x".into());

        dbg!(&tx.find_params());

        let tx = tx.apply().unwrap();

        let tx = resolve_tx(
            tx,
            MockLedger {
                default_datum: Some(tx3_lang::ir::Expression::Struct(tx3_lang::ir::StructExpr {
                    constructor: 0,
                    fields: vec![
                        tx3_lang::ir::Expression::Number(1),
                        tx3_lang::ir::Expression::Bytes(b"abc".to_vec()),
                    ],
                })),
            },
            Config::default(),
        )
        .await
        .unwrap();

        dbg!(&tx);

        assert_eq!(
            tx.hash.to_string(),
            "650a2e8f4850737f4cdcacaa6069db9eae3af64adb04811a715441c24292a76f"
        );
    }

    #[tokio::test]
    async fn env_vars_test() {
        let protocol = load_protocol("env_vars");

        let mut tx = protocol.new_tx("mint_from_env").unwrap().apply().unwrap();

        tx.set_arg("minter", "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x".into());
        tx.set_arg(
            "mint_policy",
            hex::decode("682d6d95495403b491737b95dae5c1f060498d9efc91a592962134f8")
                .unwrap()
                .into(),
        );
        tx.set_arg(
            "mint_script",
            ArgValue::UtxoRef(UtxoRef {
                txid: hex::decode(
                    "682d6d95495403b491737b95dae5c1f060498d9efc91a592962134f880398be2",
                )
                .unwrap(),
                index: 1,
            }),
        );

        dbg!(&tx.find_params());

        let tx = tx.apply().unwrap();

        let tx = resolve_tx(tx, MockLedger::default(), Config::default())
            .await
            .unwrap();

        dbg!(&tx);

        assert_eq!(
            tx.hash.to_string(),
            "d426d1054e93de90c666cbf495b79c33782e42c6259216b8463bbe138f5ac5b6"
        );
    }

    #[tokio::test]
    async fn local_vars_test() {
        let protocol = load_protocol("local_vars");

        let mut tx = protocol.new_tx("mint_from_local").unwrap().apply().unwrap();

        tx.set_arg("minter", "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x".into());
        tx.set_arg(
            "mint_policy",
            hex::decode("682d6d95495403b491737b95dae5c1f060498d9efc91a592962134f8")
                .unwrap()
                .into(),
        );
        tx.set_arg("quantity", ArgValue::Int(100_000_000));

        dbg!(&tx.find_params());

        let tx = tx.apply().unwrap();

        let tx = resolve_tx(tx, MockLedger::default(), Config::default())
            .await
            .unwrap();

        dbg!(&tx);

        assert_eq!(
            tx.hash.to_string(),
            "8cc7d414c54c3169b3179998514e19a9015eb597f2ce232b929a9b070bc63ec4"
        );
    }

    #[tokio::test]
    async fn adhoc_witness_test() {
        let protocol = load_protocol("cardano_witness");

        let mut tx = protocol
            .new_tx("mint_from_script")
            .unwrap()
            .apply()
            .unwrap();

        tx.set_arg("minter", "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x".into());
        tx.set_arg("quantity", ArgValue::Int(100_000_000));

        dbg!(&tx.find_params());

        let tx = tx.apply().unwrap();

        let tx = resolve_tx(tx, MockLedger::default(), Config::default())
            .await
            .unwrap();

        dbg!(&tx);

        assert_eq!(
            tx.hash.to_string(),
            "dd32c0fbfe556c1c497aab103867ea2cff7bd89e88d25caacd4fa69a33f20997"
        );
    }

    #[tokio::test]
    async fn extra_fees_test() {
        let protocol = load_protocol("transfer");

        let tx = protocol.new_tx("transfer")
            .unwrap()
            .with_arg("Sender", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"))
            .with_arg("Receiver", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"))
            .with_arg("quantity", ArgValue::Int(100_000_000))
            .apply()
            .unwrap();

        let extra_fees = 1_200_000;
        let config = Config {
            extra_fees: Some(extra_fees),
            ..Default::default()
        };

        let tx = resolve_tx(tx, MockLedger::default(), config).await.unwrap();

        assert!(tx.fee >= extra_fees);
    }

    #[tokio::test]
    async fn extra_fees_zero_test() {
        let protocol = load_protocol("transfer");

        let tx = protocol.new_tx("transfer")
            .unwrap()
            .with_arg("Sender", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"))
            .with_arg("Receiver", address_to_bytes("addr1qx0rs5qrvx9qkndwu0w88t0xghgy3f53ha76kpx8uf496m9rn2ursdm3r0fgf5pmm4lpufshl8lquk5yykg4pd00hp6quf2hh2"))
            .with_arg("quantity", ArgValue::Int(100_000_000))
            .apply()
            .unwrap();

        let config = Config {
            extra_fees: Some(0),
            ..Default::default()
        };

        let tx = resolve_tx(tx, MockLedger::default(), config.clone())
            .await
            .unwrap();

        assert!(!tx.payload.is_empty());
        assert!(tx.fee < DEFAULT_EXTRA_FEES);
    }
}
