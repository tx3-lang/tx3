# 1. Introduction

## 1.1 Scope, goals, non-goals

Tx3 is a domain-specific language for describing **transaction templates** on
UTxO-based blockchains. A Tx3 program declares the shape of one or more
transactions together with the parties, assets, policies, types, and
environment values they refer to. It is not a general-purpose programming
language and contains no constructs for unbounded loops, recursion, or
side-effects.

This specification defines, normatively:

- the lexical structure of Tx3 source files (§3);
- the context-free grammar of Tx3 (§4);
- the type system (§5);
- the static semantics applied by a conforming compiler (§6);
- the meaning of each `tx`-body block (§7);
- the chain-specific extension namespace currently defined (§8);
- the conformance criteria for source programs and compilers (§9).

The following are explicitly **out of scope**:

- The Tx3 Intermediate Representation (TIR). Lowering from the source
  language to TIR is informative throughout this document, but the TIR wire
  format and its semantics are specified separately by the TIR specification.
- The Transaction Resolver Protocol (TRP) and the Transaction Invocation
  Interface (TII).
- Formal mathematical semantics. Semantics in this document are given in
  prose. Where precision matters, the prose is supplemented with examples and
  with grammar fragments.
- Runtime behavior of a transaction once submitted to a chain. Tx3 describes
  what a transaction looks like, not what executing it does.
- Codegen target languages (TypeScript, Rust, Go, Python, …) emitted by the
  `trix` toolchain.

## 1.2 Conformance terminology

The key words **MUST**, **MUST NOT**, **SHOULD**, **SHOULD NOT**, and **MAY**
in this document are to be interpreted as described in
[RFC 2119](https://www.rfc-editor.org/rfc/rfc2119) and
[RFC 8174](https://www.rfc-editor.org/rfc/rfc8174), with the additional
clarification that these key words apply to:

- **Source programs.** A program MUST conform to the grammar of §4 and the
  rules of §5–§8 to be considered a *conforming Tx3 program*.
- **Compilers.** A *conforming Tx3 compiler* MUST accept every conforming
  program and reject every non-conforming one, as further qualified in §9.

Outside of these key words, requirements are stated informatively to aid
understanding and are not themselves normative.

## 1.3 Document conventions and notation

### Defined terms

A term defined by this specification appears in *italics* on first use within
a section. Defined terms are stable across the document and are case-sensitive
where they collide with grammar productions or keywords.

### Grammar notation

The syntactic grammar in §4 is given in a conventional EBNF dialect:

| Symbol            | Meaning                                                      |
| ----------------- | ------------------------------------------------------------ |
| `A ::= B`         | production: `A` is defined as `B`.                           |
| `B C`             | sequence: `B` followed by `C`.                               |
| `B \| C`          | choice: `B` or `C`.                                          |
| `B?`              | optional: zero or one `B`.                                   |
| `B*`              | repetition: zero or more `B`.                                |
| `B+`              | repetition: one or more `B`.                                 |
| `( B C )`         | grouping.                                                    |
| `"foo"`           | the literal character sequence `foo`.                        |
| `[a-z]`           | a character class.                                           |
| `// comment`      | a grammar-level comment, not part of the language.           |

Terminal symbols are written between double quotes. Nonterminal symbols are
written in `lower_snake_case`.

### Code samples

Inline Tx3 code is rendered in a fixed-width font. Multi-line samples appear
in fenced code blocks tagged `tx3`:

```tx3
party Alice;

tx greet() {
    output {
        to: Alice,
        amount: Ada(1),
    }
}
```

Samples are illustrative and not normative unless explicitly marked otherwise.

### Cross-references

Cross-references to other sections take the form `§<n>` (e.g. `§4.2`).
References to specific grammar productions name the production directly
(e.g. `<tx_def>`). References to RFCs and other external standards are given
as hyperlinks at first use.

## 1.4 Versioning policy

The specification is versioned independently of the reference implementation.
Each version lives in its own directory under `specs/` (this document is the
`v1beta0` version).

- Within a version, changes are **additive and backward compatible**. A
  program that conforms to `v1betaN` MUST continue to conform to any
  subsequent patch revision of `v1betaN`.
- A change that would cause a previously conforming program to no longer
  conform, or that would change the meaning of an existing program, is a
  **breaking change**. Breaking changes are made by creating a new version
  directory.
- The current version, `v1beta0`, is in **beta**: while breaking changes are
  not expected to be frequent, they are permitted as the language stabilises
  toward `v1`.

Implementations SHOULD advertise the highest specification version they
conform to and SHOULD document any extensions or known divergences from that
version.
