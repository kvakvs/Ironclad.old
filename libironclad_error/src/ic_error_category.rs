//! Larger categories for errors

use crate::ic_error_trait::IcErrorT;

/// General error type covering all system errors, parser errors, compiler errors, etc.
pub enum IcErrorCategory {
  /// Returned when multiple errors were found, report each error
  Multiple(Vec<Box<dyn IcErrorT>>),

  /// Returned when file or directory read/write failed
  Io(std::io::Error),

  /// Project errors produced when glob() scanning input files and directories
  Glob(glob::GlobError),

  /// Returned when directory scan glob pattern contained an error
  GlobPattern(glob::PatternError),

  /// Project loading error produced when loading TOML
  Config(toml::de::Error),

  /// Returned when preprocessor parser failed
  PreprocessorParse,

  /// Returned when preprocessor syntax is not correct
  Preprocessor,

  /// Returned when Erlang parser failed: internal error must not occur with the user
  ParserInternal,

  /// Something unexpected like a TO-DO or assertion
  Internal,

  /// Returned when Erlang parser failed
  ErlangParse,

  /// A variable was referenced that's not in the scope
  VariableNotFound(String),
}
