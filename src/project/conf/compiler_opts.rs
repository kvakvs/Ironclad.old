use serde_derive::Deserialize;
use std::fmt::Debug;

/// Options for building entire project, or a single module
#[derive(Deserialize, Debug)]
pub struct CompilerOptsConf {
  /// If not specified, defaults to empty
  pub include_paths: Option<Vec<String>>,
  /// Defaults to empty list. Preprocessor defs in form of "NAME" or "NAME=VALUE"
  pub defines: Option<Vec<String>>,
}

impl Default for CompilerOptsConf {
  fn default() -> Self {
    Self {
      include_paths: None,
      defines: None,
    }
  }
}
