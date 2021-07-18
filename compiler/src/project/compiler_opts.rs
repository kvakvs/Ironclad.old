use crate::project::conf::compiler_opts::CompilerOptsConf;

#[derive(Debug)]
pub struct CompilerOpts {
  /// If not specified, defaults to empty
  pub include_paths: Vec<String>,

  /// Defaults to empty list. Preprocessor defs in form of "NAME" or "NAME=VALUE"
  pub defines: Vec<String>,
}

impl Default for CompilerOpts {
  fn default() -> Self {
    Self {
      include_paths: vec![],
      defines: vec![],
    }
  }
}

impl From<CompilerOptsConf> for CompilerOpts {
  fn from(opts: CompilerOptsConf) -> Self {
    let self_default = Self::default();
    Self {
      include_paths: opts.include_paths.unwrap_or(self_default.include_paths),
      defines: opts.defines.unwrap_or(self_default.defines),
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
