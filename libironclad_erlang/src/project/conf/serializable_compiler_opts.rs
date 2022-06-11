//! Compiler options for each module, and for the entire project, as loaded from TOML with optional
//! fields wrapped with Option tag. Later converted to CompilerOpts with Option stripped and missing
//! fields replaced with the defaults.
use serde_derive::Deserialize;
use std::fmt::Debug;

/// Options for building entire project, or a single module
/// This version of struct is parsed from TOML and all optional fields are Option<>
/// The real config is in the module above this.
#[derive(Default, Deserialize, Debug)]
pub struct SerializableCompilerOpts {
  /// If not specified, defaults to empty
  pub include_paths: Option<Vec<String>>,
  /// Defaults to empty list. Preprocessor defs in form of "NAME" or "NAME=VALUE"
  pub defines: Option<Vec<String>>,
}
