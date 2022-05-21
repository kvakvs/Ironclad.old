//! Defines libironclad options for a file
use std::sync::Arc;
use crate::project::conf::compiler_opts::CompilerOptsConf;
use crate::stage::preprocess::pp_scope::PreprocessorScope;

/// Compiler options for a file
#[derive(Debug, Clone)]
pub struct CompilerOpts {
  /// If not specified, defaults to empty
  pub include_paths: Vec<String>,

  // pub opts: Vec<CompilerOption> ...

  /// Preprocessor macro defines in form of "NAME" or "NAME=VALUE" or NAME(ARGS...)=VALUE
  pub scope: Arc<PreprocessorScope>,

  /// Tries to break the operations when this many errors found in 1 module
  pub max_errors_per_module: usize,
}

impl CompilerOpts {
  /// Default value for max errors limit. Will try to stop compilation when this count is reached.
  pub const MAX_ERRORS_PER_MODULE: usize = 20;

  /// Given self (read-only) and other opts (read-only) combine them into self+other
  pub fn overlay(&self, other: &CompilerOpts) -> Self {
    let mut result: CompilerOpts = self.clone();

    // Overlay include paths
    for incl in other.include_paths.iter() {
      if !result.include_paths.contains(incl) {
        result.include_paths.push(incl.clone());
      }
    }

    // Overlay preprocessor defines
    let new_scope = result.scope.overlay(&other.scope);
    result.scope = new_scope;

    result
  }
}

impl Default for CompilerOpts {
  fn default() -> Self {
    Self {
      include_paths: Default::default(),
      scope: Default::default(),
      max_errors_per_module: Self::MAX_ERRORS_PER_MODULE,
    }
  }
}

impl From<CompilerOptsConf> for CompilerOpts {
  fn from(opts: CompilerOptsConf) -> Self {
    let self_default = Self::default();
    Self {
      include_paths: opts.include_paths.unwrap_or(self_default.include_paths),
      scope: PreprocessorScope::new_from_config(opts.defines, &self_default.scope),
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
