use std::path::Path;

use serde_json::{json, Map, Value};

use tx3_tir::compile::CompiledTx;
use tx3_tir::encoding::{self, AnyTir};
use tx3_tir::model::assets::CanonicalAssets;
use tx3_tir::model::core::{Utxo, UtxoRef};
use tx3_tir::reduce::ArgValue;

use crate::inputs::CanonicalQuery;
use crate::interop;
use crate::job::{QueryResolution, ResolveJob, ResolveLog, ResolveLogEntry};

pub trait DiagnosticDump {
    fn to_dump(&self) -> Value;
}

// ---------------------------------------------------------------------------
// Leaf types
// ---------------------------------------------------------------------------

impl DiagnosticDump for UtxoRef {
    fn to_dump(&self) -> Value {
        interop::utxo_ref_to_json(self)
    }
}

impl DiagnosticDump for AnyTir {
    fn to_dump(&self) -> Value {
        let (bytes, version) = match self {
            AnyTir::V1Beta0(tx) => encoding::to_bytes(tx),
        };
        json!({
            "content": hex::encode(bytes),
            "encoding": "hex",
            "version": version.to_string()
        })
    }
}

impl DiagnosticDump for CompiledTx {
    fn to_dump(&self) -> Value {
        json!({
            "payload": hex::encode(&self.payload),
            "hash": hex::encode(&self.hash),
            "fee": self.fee,
            "ex_units": self.ex_units,
        })
    }
}

impl DiagnosticDump for ArgValue {
    fn to_dump(&self) -> Value {
        interop::arg_to_json(self)
    }
}

impl DiagnosticDump for CanonicalAssets {
    fn to_dump(&self) -> Value {
        let map: Map<String, Value> = self
            .iter()
            .map(|(class, amount)| (class.to_string(), json!(amount)))
            .collect();
        Value::Object(map)
    }
}

impl DiagnosticDump for Utxo {
    fn to_dump(&self) -> Value {
        interop::utxo_to_json(self)
    }
}

// ---------------------------------------------------------------------------
// Canonical query
// ---------------------------------------------------------------------------

impl DiagnosticDump for CanonicalQuery {
    fn to_dump(&self) -> Value {
        json!({
            "address": self.address.as_ref().map(|a| hex::encode(a)),
            "min_amount": self.min_amount.as_ref().map(|a| a.to_dump()),
            "refs": self.refs.iter().map(|r| r.to_dump()).collect::<Vec<_>>(),
            "support_many": self.support_many,
            "collateral": self.collateral,
        })
    }
}

// ---------------------------------------------------------------------------
// Resolve pipeline types
// ---------------------------------------------------------------------------

impl DiagnosticDump for QueryResolution {
    fn to_dump(&self) -> Value {
        json!({
            "name": self.name,
            "query": self.query.to_dump(),
            "selection": self.selection.as_ref().map(|sel| {
                sel.iter().map(|u| u.to_dump()).collect::<Vec<_>>()
            }),
        })
    }
}

impl DiagnosticDump for ResolveLog {
    fn to_dump(&self) -> Value {
        match self {
            ResolveLog::ArgsApplied(tir) => json!({"args_applied": tir.to_dump()}),
            ResolveLog::FeesApplied(tir) => json!({"fees_applied": tir.to_dump()}),
            ResolveLog::CompilerApplied(tir) => json!({"compiler_applied": tir.to_dump()}),
            ResolveLog::Reduced(tir) => json!({"reduced": tir.to_dump()}),
            ResolveLog::InputsResolved(tir) => json!({"inputs_resolved": tir.to_dump()}),
            ResolveLog::FinalReduced(tir) => json!({"final_reduced": tir.to_dump()}),
            ResolveLog::Compiled(tx) => json!({"compiled": tx.to_dump()}),
            ResolveLog::Converged => json!("converged"),
        }
    }
}

impl DiagnosticDump for ResolveLogEntry {
    fn to_dump(&self) -> Value {
        json!({
            "round": self.round,
            "event": self.event.to_dump(),
        })
    }
}

// ---------------------------------------------------------------------------
// ResolveJob — top-level
// ---------------------------------------------------------------------------

impl DiagnosticDump for ResolveJob {
    fn to_dump(&self) -> Value {
        let args: Map<String, Value> = self
            .args
            .iter()
            .map(|(k, v)| (k.clone(), interop::arg_to_json(v)))
            .collect();

        let input_pool = self.input_pool.as_ref().map(|pool| {
            let map: Map<String, Value> = pool
                .iter()
                .map(|(r, u)| {
                    let key = format!("{}#{}", hex::encode(&r.txid), r.index);
                    (key, interop::utxo_to_json(u))
                })
                .collect();
            Value::Object(map)
        });

        json!({
            "original_tir": self.original_tir.to_dump(),
            "args": args,
            "compiler": &self.compiler,
            "round": self.round,
            "last_eval": self.last_eval.as_ref().map(|e| e.to_dump()),
            "converged": self.converged,
            "log": self.log.iter().map(|e| e.to_dump()).collect::<Vec<_>>(),
            "input_queries": self.input_queries.iter().map(|q| q.to_dump()).collect::<Vec<_>>(),
            "input_pool": input_pool,
        })
    }
}

pub fn dump_to_dir(job: &ResolveJob, dir: &Path) -> Result<String, std::io::Error> {
    std::fs::create_dir_all(dir)?;
    let dump_id = uuid::Uuid::new_v4().to_string();
    let path = dir.join(format!("resolve-job-{dump_id}.json"));
    let file = std::fs::File::create(&path)?;
    serde_json::to_writer_pretty(file, &job.to_dump())
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
    tracing::debug!(dump_id = %dump_id, path = %path.display(), "diagnostic dump written");
    Ok(dump_id)
}

#[cfg(test)]
mod tests {
    use tx3_tir::encoding::AnyTir;
    use tx3_tir::model::v1beta0 as tir;
    use tx3_tir::reduce::ArgMap;

    use crate::{
        dump::dump_to_dir,
        job::{ResolveJob, ResolveLog},
    };

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
        let dump_id = dump_to_dir(&job, &dir).expect("dump should succeed");

        let path = dir.join(format!("resolve-job-{dump_id}.json"));
        assert!(path.exists());

        let contents = std::fs::read_to_string(&path).unwrap();
        let value: serde_json::Value = serde_json::from_str(&contents).unwrap();
        assert!(value.get("compiler").is_some());

        let _ = std::fs::remove_file(&path);
    }
}
