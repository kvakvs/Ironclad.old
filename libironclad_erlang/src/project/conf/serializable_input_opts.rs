//! Project input files as parsed from TOML, optional fields wrapped in Option. Later it is
//! converted into InputOpts with Option tags stripped and replaced with the defaults.
use serde_derive::Deserialize;
use std::fmt::Debug;

/// Contains source tree from the project file, or detected from the file system
#[derive(Default, Deserialize, Debug)]
pub struct SerializableInputOpts {
  /// If not specified, defaults to *.erl
  pub files: Option<Vec<String>>,

  /// Search these directories for `-include()` and `-include_lib()`
  pub include_paths: Option<Vec<String>>,

  /// If not specified, defaults to "."
  pub directories: Option<Vec<String>>,

  /// If not specified, defaults to empty
  pub exclude_files: Option<Vec<String>>,

  /// If not specified, defaults to empty
  pub exclude_directories: Option<Vec<String>>,
}

impl SerializableInputOpts {}
