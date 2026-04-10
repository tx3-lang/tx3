use tx3_resolver::job::{ResolveJob, ResolveLog};
use tx3_tir::encoding::AnyTir;
use tx3_tir::model::v1beta0 as tir;
use tx3_tir::reduce::ArgMap;

fn dummy_tir() -> AnyTir {
    AnyTir::V1Beta0(tir::Tx {
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
    })
}

#[test]
fn dump_creates_valid_json_file() {
    let mut job = ResolveJob::new(dummy_tir(), ArgMap::new());
    job.record(ResolveLog::ArgsApplied(dummy_tir()));
    job.record(ResolveLog::Converged);

    let dir = std::env::temp_dir().join("tx3-test-dump");
    let dump_id = job.dump_to_dir(&dir).expect("dump should succeed");

    let path = dir.join(format!("resolve-job-{dump_id}.json"));
    assert!(path.exists());

    let contents = std::fs::read_to_string(&path).unwrap();
    assert!(serde_json::from_str::<serde_json::Value>(&contents).is_ok());

    let _ = std::fs::remove_file(&path);
}
