//! Defines compiler options for a file
use crate::project::conf::compiler_opts::CompilerOptsConf;

/// Compiler options for a file
#[derive(Debug)]
pub struct CompilerOpts {
  /// If not specified, defaults to empty
  pub include_paths: Vec<String>,

  /// Defaults to empty list. Preprocessor defs in form of "NAME" or "NAME=VALUE"
  pub defines: Vec<String>,

  /// Tries to break the operations when this many errors found in 1 module
  pub max_errors_per_module: usize,
}

impl CompilerOpts {
  /// Default value for max errors limit. Will try to stop compilation when this count is reached.
  pub const MAX_ERRORS_PER_MODULE: usize = 20;
}

impl Default for CompilerOpts {
  fn default() -> Self {
    Self {
      include_paths: vec![],
      defines: vec![],
      max_errors_per_module: Self::MAX_ERRORS_PER_MODULE,
    }
  }
}

impl From<CompilerOptsConf> for CompilerOpts {
  fn from(opts: CompilerOptsConf) -> Self {
    let self_default = Self::default();
    Self {
      include_paths: opts.include_paths.unwrap_or(self_default.include_paths),
      defines: opts.defines.unwrap_or(self_default.defines),
      max_errors_per_module: Self::MAX_ERRORS_PER_MODULE,
    }
  }
}

impl From<Option<CompilerOptsConf>> for CompilerOpts {
  fn from(maybe_opts: Option<CompilerOptsConf>) -> Self {
    match maybe_opts {
      None => Self::default(),
      Some(conf_val) => Self::from(conf_val)
    }
  }
}
