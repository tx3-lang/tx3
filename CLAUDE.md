# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Project Is

Tx3 is a domain-specific language (DSL) for describing transaction templates on UTxO-based blockchains, primarily Cardano. It lets dapp authors define transaction patterns (inputs, outputs, datums, redeemers) in a purely functional language that compiles to concrete blockchain transactions.

## Common Commands

```bash
# Build everything
cargo build

# Run all tests
cargo test --workspace

# Test a specific crate
cargo test -p tx3-lang
cargo test -p tx3-cardano
cargo test -p tx3-resolver
cargo test -p tx3-tir

# Run a specific test by name
cargo test -p tx3-lang -- parsing::tests::my_test_name

# Build and run the CLI compiler
cargo run --bin tx3c -- build <source.tx3>

# Check without building
cargo check --workspace
```

## Architecture: Compiler Pipeline

The project is a Rust workspace implementing a multi-stage compiler:

```
tx3c (CLI binary)
  ├── tx3-lang        → parse → analyze → lower → AST/TIR
  ├── tx3-cardano     → compile TIR to Cardano transactions
  ├── tx3-tir         → Transaction Intermediate Representation (shared IR)
  └── tx3-resolver    → UTxO selection and resolution at runtime
```

### Stage 1: Parsing (`crates/tx3-lang/src/parsing.rs`)
Uses a [pest](https://pest.rs/) PEG parser with grammar in `tx3.pest`. Produces an AST defined in `ast.rs`. Key types: `Program`, `Tx`, `Party`, `Input`, `Output`.

### Stage 2: Analyzing (`crates/tx3-lang/src/analyzing.rs`)
Type checking and semantic analysis. Returns an `AnalyzeReport` with errors and warnings.

### Stage 3: Lowering (`crates/tx3-lang/src/lowering.rs`)
Converts the AST to `tx3_tir::model::v1beta0::Tx` objects — simplifying and normalizing for the downstream compiler. Chain-specific lowering logic lives in `crates/tx3-lang/src/cardano/`.

### Stage 4: Cardano Compilation (`crates/tx3-cardano/src/`)
Implements the `tx3_tir::compile::Compiler` trait. Sub-modules:
- `compile/mod.rs` — main compilation logic
- `asset_math.rs` — asset arithmetic
- `plutus_data.rs` — Plutus data serialization
- `coercion.rs` — type coercion to Cardano types

### Stage 5: UTxO Resolution (`crates/tx3-resolver/src/`)
Runtime component. Takes a compiled transaction template and selects real UTxOs to fill inputs. Key sub-modules:
- `inputs/select/` — selection strategies (naive, vector-based)
- `inputs/narrow.rs` — UTxO filtering
- `trp/` — Transaction Resolution Protocol

### High-Level API
`tx3-lang` exposes a `Workspace` facade (`crates/tx3-lang/src/facade.rs`) that orchestrates the full parse→analyze→lower pipeline. The `include_tx3_build!` macro supports build-time compilation of `.tx3` files.

## Key Design Patterns

- **Trait-based abstraction**: `tx3_tir::compile::Compiler` is the abstract compiler interface; `tx3_resolver::UtxoStore` abstracts UTxO storage (async).
- **Error handling**: `thiserror` for error types, `miette` for pretty diagnostics.
- **Serialization**: CBOR via `ciborium` for TIR wire format; `serde` + JSON for config.
- **Test fixtures**: `examples/` contains `.tx3` source files alongside `.ast` and `.tir` snapshots showing expected intermediate representations.

## Tx3 Language Concepts

The language's core constructs:
- `party` — a participant (wallet address or script)
- `policy` — an on-chain validation script (can also act as a party)
- `record` — a data structure for datums/redeemers
- `tx` — a transaction template (parameterized function returning a transaction)
- Inside `tx`: `input` blocks (UTxO selection criteria) and `output` blocks (new UTxOs to create)
- Special values: `fees`, `Ada(quantity)`, `source` (input reference for value arithmetic)
