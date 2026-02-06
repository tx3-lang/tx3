# Implementation Plan - Support Plutus JSON Imports

The goal is to allow importing types from `plutus.json` files (CIP-57) into the Tx3 DSL using the `cardano::import` statement.

## User Review Approved

> [!NOTE]
> Coupling `tx3-lang` to the `cip-57` implementation is confirmed as acceptable.
>
> Types imported from `plutus.json` will be mapped to `tx3` `TypeDef` and `VariantCase`.
> - Product types (single constructor) -> `TypeDef` with one `VariantCase`.
> - Sum types (anyOf) -> `TypeDef` with multiple `VariantCase`s.
> - Primitive types map to `Int`, `Bytes`, `List`, `Map`.
>
> Naming and Collision Handling:
> - If `as Alias` is provided, imported types are prefixed: `Alias_TypeName`.
> - If no alias is provided, types are imported as is.
> - **CRITICAL: Any naming collision (with existing types or other imports) MUST throw an error.**

## Proposed Changes

### `crates/tx3-lang`

#### [MODIFY] `Cargo.toml`
- Add `cip-57` to `[dependencies]`.

#### [MODIFY] `tx3.pest`
- Add `import_def` rule: `import_def = { "cardano" ~ "::" ~ "import" ~ string ~ ("as" ~ identifier)? ~ ";" }`
- Add `import_def` to `program` rule.

#### [MODIFY] `ast.rs`
- Add `ImportDef` struct.
- Add `imports: Vec<ImportDef>` to `Program` struct.

#### [MODIFY] `parsing.rs`
- Implement `AstNode` for `ImportDef`.
- Update `Program::parse` to parse imports.

#### [NEW] `interop.rs`
- Create `resolve_imports(program: &mut Program, root: &Path) -> Result<(), Error>`.
- Logic:
    - Iterate `program.imports`.
    - Resolve path relative to `root`.
    - Read file (IO).
    - Parse `cip_57::Blueprint`.
    - Iterate `blueprint.definitions`.
    - Map `Definition` to `TypeDef` (flattening `Schema`s).
    - Apply alias prefix if present.
    - Append to `program.types`.
    - **Verify no collisions** with existing types or other imports.

#### [MODIFY] `facade.rs`
- Update `Workspace` struct to store `root: Option<PathBuf>`.
- Update `Workspace::from_file` to store the path.
- Update `Workspace::parse` (or a helper) to call `interop::resolve_imports` after parsing.

#### [MODIFY] `lib.rs`
- Export `interop` module.

## Verification Plan

### Automated Tests
- Create `crates/tx3-lang/tests/imports.rs`.
- Test cases:
    1. Successful import with and without alias.
    2. Import with alias prefixing.
    3. Collision detection between two imports.
    4. Collision detection between import and local type.
    5. Invalid JSON file handling.

### Manual Verification
- Use `bin/tx3c` to build a sample project.
    - Create `examples/import_test.tx3`.
    - Run `cargo run -p tx3-cli -- build examples/import_test.tx3`.
