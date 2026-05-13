# Tx3 Language Specification — `v1beta0`

> **Status:** Beta. The language surface described here is stable enough to
> implement against, but is not frozen. Breaking changes will result in a new
> version directory rather than in-place edits.

## Table of contents

1. [Introduction](01-introduction.md) — scope, conformance terminology, document conventions.
2. [Domain model](02-domain-model.md) — abstract entities the language describes.
3. [Lexical structure](03-lexical-structure.md) — source encoding, tokens, literals, keywords.
4. [Syntactic grammar](04-syntactic-grammar.md) — EBNF for the full language.
5. [Type system](05-type-system.md) — primitive, compound, and user-defined types.
6. [Static semantics](06-static-semantics.md) — scoping, name resolution, type checking, validation rules.
7. [Transaction semantics](07-transaction-semantics.md) — meaning of each `tx`-body block.
8. [Cardano extensions](08-cardano-extensions.md) — the `cardano::` block namespace.
9. [Conformance](09-conformance.md) — what it means to conform to this specification.

## Reading order

Sections are designed to be read in order. Each section assumes the previous
ones. Forward references are used sparingly; where they appear they are
explicit (e.g. "see §6.3").

## Relationship to the reference implementation

This document is the normative definition of `v1beta0`. The reference
implementation lives in
[`tx3/crates/tx3-lang`](../../crates/tx3-lang/) and currently uses a
[Pest](https://pest.rs/) grammar at
[`tx3/crates/tx3-lang/src/tx3.pest`](../../crates/tx3-lang/src/tx3.pest)
together with the Rust modules `ast.rs`, `analyzing.rs`, `cardano.rs`, and
`lowering.rs`. Pest is a parser-generator detail; the grammar in §4 is given
in EBNF and is parser-agnostic.

Known divergences between this specification and the reference implementation,
where any exist, are listed in [§9](09-conformance.md).

## Change log

- `v1beta0` — initial specification, transcribed from the reference
  implementation at the `tx3-lang/tx3` v0.17 line.
