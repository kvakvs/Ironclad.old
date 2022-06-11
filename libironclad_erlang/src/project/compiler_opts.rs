//! Defines libironclad options for a file
use crate::erl_syntax::parsers::parser_scope::{ParserScopeImpl, PreprocessorDefinesMap};
use crate::project::conf::serializable_compiler_opts::SerializableCompilerOpts;

/// Compiler options for a file
#[derive(Debug, Clone)]
pub struct CompilerOpts {
  /// If not specified, defaults to empty
  pub include_paths: Vec<String>,

  // pub opts: Vec<CompilerOption> ...
  /// Preprocessor macro defines in form of "NAME" or "NAME=VALUE" or NAME(ARGS...)=VALUE
  pub scope: PreprocessorDefinesMap,

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
    let new_scope =
      // if let (Ok(r_result_scope), Ok(r_other_scope)) = (result.scope.read(), other.scope.read()) {
        ParserScopeImpl::overlay(&result.scope, &other.scope);
    // } else {
    //   panic!("Can't lock scopes for merging")
    // };
    result.scope = new_scope;
    result
  }

  pub(crate) fn new_from_opts(opts: SerializableCompilerOpts) -> Self {
    let self_default = Self::default();
    Self {
      include_paths: opts.include_paths.unwrap_or(self_default.include_paths),
      scope: ParserScopeImpl::new_from_config(opts.defines, &self_default.scope),
      max_errors_per_module: Self::MAX_ERRORS_PER_MODULE,
    }
  }

  pub(crate) fn new_from_maybe_opts(maybe_opts: Option<SerializableCompilerOpts>) -> Self {
    match maybe_opts {
      None => Self::default(),
      Some(conf_val) => CompilerOpts::new_from_opts(conf_val),
    }
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