//! Compile input options: All input directories, glob masks for file scanning, include/exclude etc
use crate::project::conf::input_opts::InputOptsConf;
use std::fmt::Debug;

/// Same as InputOptsConf but no Option<> fields
/// Contains options defining libironclad inputs
#[derive(Debug)]
pub struct InputOpts {
  /// If not specified, defaults to *.erl
  pub files: Vec<String>,

  /// If not specified, defaults to "."
  pub directories: Vec<String>,

  /// Search these directories for `-include()` and `-include_lib()`
  pub include_paths: Vec<String>,

  /// If not specified, defaults to empty
  #[allow(dead_code)]
  pub exclude_files: Vec<String>,

  /// If not specified, defaults to empty
  #[allow(dead_code)]
  pub exclude_directories: Vec<String>,
}

impl Default for InputOpts {
  fn default() -> Self {
    Self {
      files: vec![String::from("*.erl")],
      directories: vec![String::from(".")],
      include_paths: vec![],
      exclude_files: vec![],
      exclude_directories: vec![],
    }
  }
}

impl From<InputOptsConf> for InputOpts {
  fn from(opts: InputOptsConf) -> Self {
    let self_default = Self::default();
    Self {
      files: opts.files.unwrap_or(self_default.files),
      include_paths: opts.include_paths.unwrap_or(self_default.include_paths),
      directories: opts.directories.unwrap_or(self_default.directories),
      exclude_files: opts.exclude_files.unwrap_or(self_default.exclude_files),
      exclude_directories: opts
        .exclude_directories
        .unwrap_or(self_default.exclude_directories),
    }
  }
}

impl From<Option<InputOptsConf>> for InputOpts {
  fn from(maybe_opts: Option<InputOptsConf>) -> Self {
    match maybe_opts {
      None => Self::default(),
      Some(conf_val) => Self::from(conf_val),
    }
  }
}
