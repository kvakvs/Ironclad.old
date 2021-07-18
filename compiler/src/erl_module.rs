//! Defines an Erlang module ready to be compiled

use crate::project::compiler_opts::CompilerOpts;
use std::fmt::Debug;
use std::fmt;
use std::sync::Arc;

/// Erlang Module consists of
/// - List of forms: attributes, and Erlang functions
/// - Compiler options used to produce this module
pub struct ErlModule {
  /// Options used to build this module. Possibly just a ref to the main project's options
  compiler_options: Arc<CompilerOpts>,
  /// Module name atom, as a string
  name: String,
}

impl Default for ErlModule {
  fn default() -> Self {
    Self {
      compiler_options: Default::default(),
      name: "".to_string(),
    }
  }
}


impl Debug for ErlModule {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "ErlModule({})", self.name)
  }
}
