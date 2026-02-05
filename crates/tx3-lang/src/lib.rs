//! The Tx3 language
//!
//! This crate provides the parser, analyzer and lowering logic for the Tx3
//! language.
//!
//! # Parsing
//!
//! ```
//! let program = tx3_lang::parsing::parse_string("tx swap() {}").unwrap();
//! ```
//!
//! # Analyzing
//!
//! ```
//! let mut program = tx3_lang::parsing::parse_string("tx swap() {}").unwrap();
//! tx3_lang::analyzing::analyze(&mut program).ok().unwrap();
//! ```
//!
//! # Lowering
//!
//! ```
//! let mut program = tx3_lang::parsing::parse_string("tx swap() {}").unwrap();
//! tx3_lang::analyzing::analyze(&mut program).ok().unwrap();
//! let ir = tx3_lang::lowering::lower(&program, "swap").unwrap();
//! ```

pub mod analyzing;
pub mod ast;
pub mod lowering;
pub mod parsing;

// chain specific
pub mod cardano;

#[macro_export]
macro_rules! include_tx3_build {
    ($package: tt) => {
        include!(concat!(env!("OUT_DIR"), concat!("/", $package, ".rs")));
    };
}

mod facade;
pub use facade::{Error, Workspace};
