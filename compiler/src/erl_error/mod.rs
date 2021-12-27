//! Contains all possible Erlang compiler errors
use std::path::{Path};

use pest::error::LineColLocation;

use crate::source_loc::{ErrorLocation, SourceLoc};
use crate::mfarity::MFArity;
use crate::preprocessor::syntax_tree::pp_parser;
use crate::typing::type_error::TypeError;

pub mod format;
pub mod from;

/// Erlang compiler errors all gathered together
pub enum ErlError {
  /// Returned when multiple errors were found, report each error
  Interrupted(String),
  /// Returned when multiple errors were found, report each error
  Multiple(Vec<ErlError>),
  /// Returned when file or directory read/write failed
  Io(std::io::Error),
  /// Project errors produced when glob() scanning input files and directories
  Glob(glob::GlobError),
  /// Returned when directory scan glob pattern contained an error
  GlobPattern(glob::PatternError),
  /// Project loading error produced when loading TOML
  Config(toml::de::Error),

  /// Returned when preprocessor parser failed
  PreprocessorParse {
    /// Location where error was found
    loc: ErrorLocation,
    /// Message from the compiler
    msg: String,
  },

  /// Returned when preprocessor syntax is not correct
  PreprocessorSyntax {
    /// Error from the PEST parser
    parse_err: pest::error::Error<pp_parser::Rule>
  },

  /// Returned when Erlang parser failed: internal error must not occur with the user
  ParserInternal {
    /// Some hint at where the error has occured
    loc: ErrorLocation,
    /// Message from the compiler
    msg: String,
  },

  /// Something unexpected like a TO-DO or assertion
  Internal(String),

  /// Returned when Erlang parser failed
  ErlangParse {
    /// Some hint at where the error has occured
    loc: ErrorLocation,
    /// Message from the compiler
    msg: String,
  },

  // /// Returned when Erlang syntax is not correct
  // ErlangSyntax {
  //   /// Error from PEST parser
  //   parse_err: pest::error::Error<erl_parser_prec_climber::Rule>,
  //   /// Message from the compiler
  //   msg: String,
  // },

  /// A variable was referenced that's not in the scope
  VariableNotFound(String),
  /// A local function referenced by MFA (module ignored) is not found
  LocalFunctionNotFound(MFArity),
  /// Returned when a type error or mismatching types were found
  TypeErr(TypeError),
}

impl ErlError {
  /// Some errors might result in a non-0 exit code, list them here
  pub fn process_exit_code(&self) -> i32 { 1 }

  /// Create an internal error
  pub fn internal<T>(message: String) -> ErlResult<T> {
    Err(ErlError::Internal(message))
  }

  /// Wraps a `TypeError`
  pub fn type_error<T>(terr: TypeError) -> ErlResult<T> {
    Err(ErlError::TypeErr(terr))
  }

  /// Wraps a `VariableNotFound`
  pub fn variable_not_found<T>(var_name: &str) -> ErlResult<T> {
    Err(ErlError::VariableNotFound(String::from(var_name)))
  }

  /// Wraps a `FunctionNotFound`
  pub fn local_function_not_found<T>(mfa: &MFArity) -> ErlResult<T> {
    Err(ErlError::LocalFunctionNotFound(mfa.clone()))
  }

  /// Creates a preprocessor error from a filename and a message
  pub fn pp_parse<T>(file_name: &Path, message: &str) -> ErlResult<T> {
    Err(ErlError::PreprocessorParse {
      loc: ErrorLocation::new(Some(file_name.to_path_buf()), SourceLoc::None),
      msg: String::from(message),
    })
  }

  /// Formats a linecol-location from Pest nicely
  fn format_line_col(p: &LineColLocation) -> String {
    match p {
      LineColLocation::Pos((l, c)) => format!("{}:{}", l, c),
      LineColLocation::Span((l, c), (l2, c2)) => {
        format!("{}:{} .. {}:{}", l, c, l2, c2)
      }
    }
  }

  /// Create a parser internal error. Should not happen for the user, only during the development
  /// and testing.
  pub fn parser_internal(location: SourceLoc, msg: String) -> Self {
    ErlError::ParserInternal {
      loc: ErrorLocation {
        path: None,
        location,
      },
      msg,
    }
  }

  /// Given a vector of ErlErrors, return one, multiple error, or panic if no errors were given
  pub fn multiple(mut errors: Vec<ErlError>) -> ErlError {
    match errors.len() {
      0 => panic!("ErlError::multiple() called with an empty error vector"),
      1 => errors.pop().unwrap(),
      _ => ErlError::Multiple(errors),
    }
  }
}

/// Used as Result<T> for all parse and compile operations
pub type ErlResult<T> = Result<T, ErlError>;
