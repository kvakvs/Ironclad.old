//! Larger categories for errors

use crate::error::ic_error_trait::GenericIroncladError;
use crate::typing::type_error::TypeError;
use libironclad_util::io::file_error::IcFileError;
use std::path::PathBuf;

/// General error type covering all system errors, parser errors, compiler errors, etc.
#[derive(Debug)]
pub enum IcErrorKind {
  /// Returned when multiple errors were found, report each error
  Multiple(Vec<GenericIroncladError>),

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

  /// Something unexpected like a TO-DO or assertion
  Internal,

  /// Returned when processing the Erlang program and finding problems like missing symbols
  Erlang,

  /// Some type discrepancy has been detected
  TypeErr,

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

impl IcErrorKind {
  /// Converts category to printable
  pub(crate) fn to_string(&self) -> &str {
    match self {
      IcErrorKind::Multiple(_) => "Multiple errors",
      IcErrorKind::StdIoError(_) => "IO (general) error",
      IcErrorKind::IcFileError(_) => "IO (file) error",
      IcErrorKind::Glob(_) => "Directory scan error",
      IcErrorKind::GlobPattern(_) => "Glob pattern error",
      IcErrorKind::Config(_) => "Configuration file error",
      IcErrorKind::Internal => "Internal error",
      IcErrorKind::VariableNotFound(_) => "Variable not found",
      IcErrorKind::TypeErr { .. } => "Type error",
      IcErrorKind::Erlang => "Program structure error",
      IcErrorKind::FileNotFound { .. } => "File was not found",
    }
  }
}
