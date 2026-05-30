# Tx3 Language Specification

This directory is the canonical home of the **normative specification** of the
Tx3 source language.

## Scope

The specification covers the **high-level Tx3 source language**: the surface
syntax authored in `.tx3` files, its type system, and the static semantics that
a conforming compiler applies before lowering. It does **not** cover:

- The Tx3 Intermediate Representation (TIR). TIR is specified separately under
  [`tx3-lang/tir`](https://github.com/tx3-lang/tir).
- The Transaction Resolver Protocol (TRP). See
  [`tx3-lang/trp`](https://github.com/tx3-lang/trp).
- The Transaction Invocation Interface (TII). See
  [`tx3-lang/tii`](https://github.com/tx3-lang/tii).
- Codegen frontends, the `trix` toolchain, devnet, or any client SDK.

## Source of truth

The specification under this directory is the **normative** definition of the
language. Where it disagrees with the reference Rust implementation in
[`crates/tx3-lang/`](../crates/tx3-lang/), the implementation is the one in
error. The implementation will be brought into alignment over time.

Until that alignment is complete, the documents under each version directory
also note any known divergences (see each `09-conformance.md`).

## Versions

| Version    | Status  | Directory              |
| ---------- | ------- | ---------------------- |
| `v1beta0`  | current | [`v1beta0/`](v1beta0/) |

A new version directory is created when a breaking change to the language is
introduced. Within a version, changes are additive and backward compatible. See
each version's `01-introduction.md` for its versioning policy.

## Audience

- Implementers of Tx3 compilers, analyzers, formatters, language servers, and
  alternative toolchains.
- Reviewers proposing changes to the language.
- Authors writing portable Tx3 programs who need a precise reference for what
  a conforming compiler MUST accept.

For tutorial-style material aimed at end-user authors of `.tx3` programs, see
the docs under [`tx3-lang/docs`](https://github.com/tx3-lang/docs).
