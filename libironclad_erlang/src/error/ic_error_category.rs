//! Larger categories for errors

use crate::error::ic_error_trait::IcError;
use libironclad_util::io::file_error::IcFileError;
use std::path::PathBuf;

/// General error type covering all system errors, parser errors, compiler errors, etc.
#[derive(Debug)]
pub enum IcErrorCategory {
  /// Returned when multiple errors were found, report each error
  Multiple(Vec<IcError>),

  /// Returned when file or directory read/write failed
  StdIoError(std::io::Error),
  /// Ironclad's own File IO error reporting
  IcFileError(IcFileError),

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

  /// Returned when processing the Erlang program and finding problems like missing symbols
  Erlang,

  /// Some type discrepancy has been detected
  TypeError,

  /// A variable was referenced that's not in the scope
  VariableNotFound(String),

  /// File loading failed for whatever reason
  FileNotFound {
    /// The file which was searched
    file: PathBuf,
    /// The action which failed due to a missing file
    while_verb: String,
  },
}

impl IcErrorCategory {
  /// Converts category to printable
  pub(crate) fn to_string(&self) -> &str {
    match self {
      IcErrorCategory::Multiple(_) => "Multiple errors",
      IcErrorCategory::StdIoError(_) => "IO (general) error",
      IcErrorCategory::IcFileError(_) => "IO (file) error",
      IcErrorCategory::Glob(_) => "Directory scan error",
      IcErrorCategory::GlobPattern(_) => "Glob pattern error",
      IcErrorCategory::Config(_) => "Configuration file error",
      IcErrorCategory::Preprocessor => "Preprocessor error",
      IcErrorCategory::PreprocessorParse => "Preprocessor parse error",
      IcErrorCategory::ParserInternal => "Parser internal error",
      IcErrorCategory::Internal => "Internal error",
      IcErrorCategory::ErlangParse => "Erlang parse error",
      IcErrorCategory::VariableNotFound(_) => "Variable not found",
      IcErrorCategory::TypeError => "Type error",
      IcErrorCategory::Erlang => "Program structure error",
      IcErrorCategory::FileNotFound { .. } => "File was not found",
    }
  }
}
