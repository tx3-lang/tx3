# Changelog

All notable changes to this project will be documented in this file.

## [0.12.0] - 2025-11-10

### 🚀 Features

- *(cardano)* Implement treasury donation block (#222)
- Support accessing list elements via expressions (#234)
- Add tip slot compiler op (#236)
- Support maps as new type / expression (#204)
- *(cardano)* Support publish of reference scripts (#208)
- *(lang)* Support type aliases (#240)
- Introduce time / slot conversion functions (#241)
- *(lang)* Introduce optional outputs (#244)
- *(lang)* Add metadata validation to enforce 64-byte limit (#238)

### 🐛 Bug Fixes

- *(resolver)* Use correct subset for naked utxo search space (#232)
- *(cardano)* Add missing burn redeemers to tx compilation (#237)
- *(lang)* Show descriptive errors for analysis report (#231)

### 🚜 Refactor

- *(lang)* Switch to cbor encoding for IR bytes (#247)

### 🧪 Testing

- *(lang)* Fix flaky span test (#250)

### ⚙️ Miscellaneous Tasks

- Implement code coverage report (#235)
- Update lang tour with recent updates (#243)

## [0.11.4] - 2025-08-19

### 🐛 Bug Fixes

- *(resolver)* Compute search space intersection correctly (#227)

## [0.11.3] - 2025-08-15

### 🐛 Bug Fixes

- *(resolver)* Have more flexibility in the naive accumulator (#226)

### 🧪 Testing

- Fix outdated tx hashes

## [0.11.2] - 2025-08-13

### 🚀 Features

- Support concat op on lists (#223)

### 🐛 Bug Fixes

- *(resolver)* Support querying for 0 assets
- *(resolver)* Allow collateral selection overlap (#221)
- Default to plutus v3 (#224)

### 🚜 Refactor

- *(resolver)* Make search space struct public

## [0.11.0] - 2025-07-29

### 🚀 Features

- Support min utxo calculation as a language feature (#216)

### 🐛 Bug Fixes

- Fix edge cases in canonical asset math (#217)
- *(resolver)* Don't repeat utxos during input selection (#172)

### 🚜 Refactor

- *(resolver)* Split search space from selection (#219)

## [0.10.1] - 2025-07-22

### 🐛 Bug Fixes

- *(resolver)* Return silently on max rounds

## [0.10.0] - 2025-07-22

### 🚀 Features

- Support selection of many utxos per input (#213)
- Finish implementation for asset burns (#214)

### 🐛 Bug Fixes

- *(parsing)* Make concat expression precede less specific rules (#210)
- Use special coin selection for collateral (#215)

### 🚜 Refactor

- Introduce backend abstraction (#211)
- Introduce visitor pattern for compiler ops (#212)
- Rename backend module

### ⚙️ Miscellaneous Tasks

- Remove legacy tx3-test crate
- Fix lint warnings
- Bump TIR version to v1alpha8

## [0.9.0] - 2025-07-18

### 🚀 Features

- Implement concat operation (#203)
- *(cardano)* Support native script witnesses (#209)

## [0.8.2] - 2025-07-11

### 🐛 Bug Fixes

- *(cardano)* Improve coercion of signers (#201)

## [0.8.1] - 2025-07-10

### 🐛 Bug Fixes

- Bump TIR version to flag breaking changes

## [0.8.0] - 2025-07-10

### 🚀 Features

- *(cardano)* Introduce extra fee config (#193)
- Introduce compiler ops (#196)
- *(cardano)* Provide tx hash as resolve output (#199)

### 🐛 Bug Fixes

- *(cardano)* Improve eval pass logic (#195)
- Implement apply for compiler op (#197)
- Resolve collateral using new param approach (#198)
- *(cardano)* Use Plutus version in witnesses for script data hash (#200)

### 🚜 Refactor

- Improve errors for better miette support (#192)

## [0.7.2] - 2025-07-07

### 🐛 Bug Fixes

- *(applying)* Apply fees to pending input queries (#187)
- *(cardano)* Handle validity with none expressions (#188)

## [0.7.1] - 2025-07-05

### 🐛 Bug Fixes

- Use new Param IR for input queries (#186)

## [0.7.0] - 2025-07-03

### 🚀 Features

- Use pratt parser for data expressions (#158)
- Implement withdraw block (#156)
- Introduce env vars (#175)
- Introduce local vars (#176)
- *(cardano)* Support adhoc Plutus witnesses (#179)

### 🐛 Bug Fixes

- Fix several issues across the compilation stack (#150)
- Support property access of lowered expressions (#157)
- *(cardano)* Use constructor to represent a bool in Plutus data (#167)
- Treat definition names as identifiers (#177)

### 🚜 Refactor

- Unify code style for adhoc directives (#171)
- Simplify applying by using composite trait (#173)
- Provide context during lowering (#170)

### 🧪 Testing

- Cover inputs with datum scenario (#174)
- Cover program-level parsing (#178)

### ⚙️ Miscellaneous Tasks

- Bump TIR version to v1alpha6 (#180)
- Introduce cliff for changelog generation (#181)
- Move release config to file

## [0.6.0] - 2025-06-06

### 🚀 Features

- Implement `validity` and `metadata` blocks (#110)
- Add support for expressions in asset definitions (#132)
- Implement required signers block (#144)

### 🐛 Bug Fixes

- Rename tx validity fields (#135)
- *(cardano)* Skip auxiliary_data if no relevant data (#136)

## [0.5.0] - 2025-05-21

### 🚀 Features

- Support multiple mint blocks (#101)

### 🐛 Bug Fixes

- *(cardano)* Support coerce of hex into bytes (#93)
- Re-export Pallas for downstream usage (#103)
- Support disordered tx blocks (#106)

### ⚙️ Miscellaneous Tasks

- Fix lint warnings (#105)
- Formalize OpenRPC spec for the TRP (#104)

## [0.4.1] - 2025-05-10

### ⚙️ Miscellaneous Tasks

- Remove legacy code (#92)

## [0.4.0] - 2025-05-09

### 🚀 Features

- *(cardano)* Expose coercion api (#87)
- Introduce list type (#90)

### 🐛 Bug Fixes

- *(cardano)* Use correct index for hardcoded plutus v2 (#89)

### 🚜 Refactor

- *(cardano)* Improve errors during input resolution (#88)

### 🧪 Testing

- *(cardano)* Fix plutus versions in resolve tests (#91)

## [0.3.0] - 2025-05-02

### 🚀 Features

- Implement reference and collateral inputs (#80)

### 🚜 Refactor

- *(cardano)* Compute script data hash using Pallas (#85)

### 📚 Documentation

- Add real-world, not-yet-working examples (#79)
- Add order book with bugs and hardcoded always true (#78)

### ⚙️ Miscellaneous Tasks

- Update Pallas to v1.0.0-alpha.2 (#86)

## [0.2.0] - 2025-04-18

### 🚀 Features

- Implement naive eval mechanism (#3)
- Introduce IR (#5)
- Implement minimal vm for basic transfer example (#6)
- Improve type definitions (#7)
- Support chain-specific artifacts (#13)
- Introduce tx3-js library (#15)
- Implement typescript bindgen (#16)
- Support reference inputs (#19)
- Implement rollup plugin (#40)
- Implement bindgen vite plugin (#41)
- Support spend redeemers (#43)
- *(bindgen)* Support dynamic environments (#44)
- Improve analyze feedback (#47)
- *(vscode)* Add document symbol preview panel (#57)
- *(vscode)* Add resolve panel (#61)
- *(docs)* Update docs to include metadata (#62)
- *(vscode)* Improve resolve panel frontend and added form validations (#63)
- *(vscode)* Add trp servers to config (#66)
- *(vscode)* Add extension diagram preview (#67)
- *(lang)* Finish lowering todos (#68)
- Introduce AnyAsset constructor (#76)

### 🐛 Bug Fixes

- Make vesting example parse work (#9)
- Make vesting example evaluation work (#10)
- *(bindgen)* Fix conflict in cli args (#59)
- Lower input redeemers correctly (#77)

### 🚜 Refactor

- Tidy up parser and ast (#1)
- Split VM into its own crate (#4)
- Improve API surface (#11)
- Introduce ir-level applying (#12)
- Revisit crate organization (#14)
- *(bindgen)* Use IR for param reflection (#56)

### 📚 Documentation

- Add better examples to README (#8)
- Add documentation first pass (#17)
- Remove experimental label (#73)

### ⚙️ Miscellaneous Tasks

- Define scope
- Scaffold eval vm (#2)
- *(bindgen)* Prepare vite / rollup plugins for publish (#42)
- Bump rollup / vite plugins (#45)
- Scaffold vscode extension (#46)
- Decouple ledger interface from U5C (#58)
- *(vscode)* Update metadata for marketplace publish (#65)
- Remove migrated vscode extension code (#69)
- Remove migrated docs (#70)
- Remove migrated web-sdk (#71)
- Update metadata for publishing (#72)
- Update Pallas to v1.0.0-alpha.1 (#74)
- Remove migrated LSP code (#75)
- Setup release metadata

<!-- generated by git-cliff -->
