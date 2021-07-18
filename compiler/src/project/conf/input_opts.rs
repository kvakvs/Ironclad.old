//! Project input files as parsed from TOML, optional fields wrapped in Option. Later it is
//! converted into InputOpts with Option tags stripped and replaced with the defaults.
use serde_derive::Deserialize;
use std::fmt::Debug;

/// Contains source tree from the project file, or detected from the file system
#[derive(Deserialize, Debug)]
pub struct InputOptsConf {
  /// If not specified, defaults to *.erl
  pub files: Option<Vec<String>>,

  /// If not specified, defaults to "."
  pub directories: Option<Vec<String>>,

  /// If not specified, defaults to empty
  pub exclude_files: Option<Vec<String>>,

  /// If not specified, defaults to empty
  pub exclude_directories: Option<Vec<String>>,
}

impl InputOptsConf {
  // /// Given list of names check if some of them are directories, build a tree recursively.
  // pub fn from_list<I>(names: I) -> SourceTree
  //     where I: IntoIterator<Item=Path> {
  //     Self {
  //         files: vec![],
  //         directories: vec![],
  //     }
  // }
}

impl Default for InputOptsConf {
  fn default() -> Self {
    Self {
      files: None,
      directories: None,
      exclude_files: None,
      exclude_directories: None,
    }
  }
}
