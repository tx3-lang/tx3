//! Diagnostics rendering for `tx3c`.
//!
//! Two formats share one pipeline. `human` is for people running `tx3c`
//! directly (miette's fancy report). `json` is the machine contract consumed
//! by orchestrators such as `trix`, which reconstruct their own rendering from
//! it. Its shape is part of the `tx3c` CLI contract; consumers gate on the
//! `tx3c` binary version rather than an in-band marker, so changes here must
//! be coordinated with a version bump (see `trix`'s `MIN_TX3C_VERSION`).

use clap::ValueEnum;
use miette::Diagnostic;
use serde::Serialize;

use tx3_lang::analyzing;

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum DiagnosticsFormat {
    Human,
    Json,
}

#[derive(Serialize)]
struct Span {
    start: usize,
    end: usize,
}

#[derive(Serialize)]
struct DiagnosticJson {
    severity: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    code: Option<String>,
    message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    span: Option<Span>,
}

#[derive(Serialize)]
struct DiagnosticsEnvelope {
    diagnostics: Vec<DiagnosticJson>,
}

fn code_of(d: &dyn Diagnostic) -> Option<String> {
    d.code().map(|c| c.to_string())
}

fn from_analysis_error(e: &analyzing::Error) -> DiagnosticJson {
    let span = e.span();
    // The dummy span is (0, 0); only emit a real one.
    let span = (span.start != span.end).then(|| Span {
        start: span.start,
        end: span.end,
    });

    DiagnosticJson {
        severity: "error",
        code: code_of(e),
        message: e.to_string(),
        span,
    }
}

fn print_json(diagnostics: Vec<DiagnosticJson>) {
    let envelope = DiagnosticsEnvelope { diagnostics };
    // Single line, stdout: the orchestrator captures this stream.
    println!(
        "{}",
        serde_json::to_string(&envelope).expect("diagnostics serialize")
    );
}

/// Report a phase failure that aborts the build (e.g. a parse error). In JSON
/// mode this emits the uniform envelope and exits non-zero; in human mode it
/// returns the error so the caller's `?` surfaces it normally.
pub fn report_fatal(
    format: DiagnosticsFormat,
    err: tx3_lang::Error,
) -> anyhow::Result<()> {
    match format {
        DiagnosticsFormat::Json => {
            print_json(vec![DiagnosticJson {
                severity: "error",
                code: code_of(&err),
                message: err.to_string(),
                span: None,
            }]);
            std::process::exit(1);
        }
        DiagnosticsFormat::Human => Err(err.into()),
    }
}

/// Report the analyzer's collected errors. In JSON mode the envelope is always
/// emitted (even when empty) so consumers get a uniform contract, and the
/// process exits non-zero when there are errors. In human mode a non-empty set
/// is rendered with miette's fancy report; the caller still aborts afterwards.
pub fn report_analysis(
    format: DiagnosticsFormat,
    errors: &[analyzing::Error],
) -> anyhow::Result<()> {
    match format {
        DiagnosticsFormat::Json => {
            let diagnostics = errors.iter().map(from_analysis_error).collect();
            print_json(diagnostics);
            if !errors.is_empty() {
                std::process::exit(1);
            }
            Ok(())
        }
        DiagnosticsFormat::Human => {
            if !errors.is_empty() {
                let report = analyzing::AnalyzeReport {
                    errors: errors.to_vec(),
                };
                eprintln!("{:?}", miette::Report::new(report));
            }
            Ok(())
        }
    }
}
