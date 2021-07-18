//! Contains all possible Erlang compiler errors
use std::path::{Path, PathBuf};

use thiserror::Error;

use crate::syntaxtree::pp::pp_parser;
use crate::syntaxtree::erl::erl_parser;
use crate::typing::error::TypeError;
use std::num::ParseIntError;
use pest::error::LineColLocation;
use std::rc::Rc;
use crate::syntaxtree::erl::erl_ast::ErlAst;

/// Shows to the user where the error was found
#[derive(Debug)]
pub struct ErrorLocation {
  /// If we know the file where this happened
  pub path: Option<PathBuf>,
  /// If we know where in file this happened
  pub ast: Option<Rc<ErlAst>>,
}

impl ErrorLocation {
  /// Creates a new error location for filename and possibly AST location
  pub fn new(filename: Option<PathBuf>,
             ast: Option<Rc<ErlAst>>) -> ErrorLocation {
    Self {
      path: filename,
      ast,
    }
  }
}

/// Erlang compiler errors all gathered together
#[derive(Error, Debug)]
pub enum ErlError {
  /// Returned when multiple errors were found, report each error
  #[error("Multiple errors: {0:?}")]
  Multiple(Vec<ErlError>),

  // #[error("Not implemented: {explanation}")]
  // NotImpl { explanation: String },

  /// Returned when file or directory read/write failed
  #[error("File IO error: {0:?}")]
  Io(std::io::Error),

  /// Project errors produced when glob() scanning input files and directories
  #[error("Directory scan error: {0:?}")]
  Glob(glob::GlobError),

  /// Returned when directory scan glob pattern contained an error
  #[error("Glob pattern error: {0:?}")]
  GlobPattern(glob::PatternError),

  /// Project loading error produced when loading TOML
  #[error("Configuration file syntax error: {0:?}")]
  Config(toml::de::Error),

  /// Returned when preprocessor parser failed
  #[error("Preprocessor parse error: {msg} (at {loc:?})")]
  PreprocessorParse {
    /// Location where error was found
    loc: ErrorLocation,
    /// Message from the compiler
    msg: String,
  },

  /// Returned when preprocessor syntax is not correct
  #[error("Preprocessor syntax parse error: {parse_err:?}")]
  PreprocessorSyntax {
    /// Error from the PEST parser
    parse_err: pest::error::Error<pp_parser::Rule>
  },

  /// Returned when Erlang parser failed
  #[error("Erlang parse error: {msg} (at {loc:?})")]
  ErlangParse {
    /// Some hint at where the error has occured
    loc: ErrorLocation,
    /// Message from the compiler
    msg: String,
  },

  /// Returned when Erlang syntax is not correct
  #[error("Erlang syntax parse error: {msg}")]
  ErlangSyntax {
    /// Error from PEST parser
    parse_err: pest::error::Error<erl_parser::Rule>,
    /// Message from the compiler
    msg: String,
  },

  /// Returned when a type error or mismatching types were found
  #[error("Type error: {0:?}")]
  TypeError(TypeError),
}

impl ErlError {
  // pub(crate) fn not_impl<T>(what: &str) -> ErlResult<T> {
  //   Err(ErlError::NotImpl { explanation: what.to_string() })
  // }

  /// Creates a preprocessor error from a filename and a message
  pub fn pp_parse<T>(file_name: &Path, message: &str) -> ErlResult<T> {
    Err(ErlError::PreprocessorParse {
      loc: ErrorLocation::new(Some(file_name.to_path_buf()), None),
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
}

/// Used as Result<T> for all parse and compile operations
pub type ErlResult<T> = Result<T, ErlError>;

impl From<std::io::Error> for ErlError {
  fn from(value: std::io::Error) -> Self {
    ErlError::Io(value)
  }
}

impl From<toml::de::Error> for ErlError {
  fn from(value: toml::de::Error) -> Self {
    ErlError::Config(value)
  }
}

impl From<glob::GlobError> for ErlError {
  fn from(value: glob::GlobError) -> Self {
    ErlError::Glob(value)
  }
}

impl From<glob::PatternError> for ErlError {
  fn from(value: glob::PatternError) -> Self {
    ErlError::GlobPattern(value)
  }
}

impl From<pest::error::Error<pp_parser::Rule>> for ErlError {
  fn from(value: pest::error::Error<pp_parser::Rule>) -> Self {
    ErlError::PreprocessorSyntax { parse_err: value }
  }
}

impl From<pest::error::Error<erl_parser::Rule>> for ErlError {
  fn from(value: pest::error::Error<erl_parser::Rule>) -> Self {
    let msg = format!("{}", value.to_string());
    ErlError::ErlangSyntax {
      parse_err: value,
      msg,
    }
  }
}

impl From<TypeError> for ErlError {
  fn from(value: TypeError) -> Self {
    ErlError::TypeError(value)
  }
}

impl From<ParseIntError> for ErlError {
  fn from(pie: ParseIntError) -> Self {
    ErlError::ErlangParse {
      loc: ErrorLocation::new(None, None),
      msg: format!("Cannot parse integer: {}", pie.to_string()),
    }
  }
}