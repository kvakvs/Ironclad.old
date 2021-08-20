//! Contains all possible Erlang compiler errors
use std::fmt::Formatter;
use std::num::ParseIntError;
use std::path::{Path};

use pest::error::LineColLocation;

use crate::source_loc::{ErrorLocation, SourceLoc};
use crate::erlang::syntax_tree::erl_parser;
use crate::preprocessor::syntax_tree::pp_parser;
use crate::typing::error::TypeError;

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

  /// Returned when Erlang parser failed
  ErlangParse {
    /// Some hint at where the error has occured
    loc: ErrorLocation,
    /// Message from the compiler
    msg: String,
  },

  /// Returned when Erlang syntax is not correct
  ErlangSyntax {
    /// Error from PEST parser
    parse_err: pest::error::Error<erl_parser::Rule>,
    /// Message from the compiler
    msg: String,
  },

  /// Returned when a type error or mismatching types were found
  TypeError(TypeError),
}

impl std::fmt::Debug for ErlError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self) }
}

impl std::fmt::Display for ErlError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      ErlError::Interrupted(s) => write!(f, "Processing interrupted: {}", s),
      ErlError::Multiple(errs) => {
        writeln!(f, "Multiple errors:")?;
        for err in errs.iter() {
          writeln!(f, "{}", err)?;
        }
        Ok(())
      }
      ErlError::Io(ioerr) => write!(f, "File IO error: {}", ioerr),
      ErlError::Glob(gerr) => write!(f, "Directory scan error: {}", gerr),
      ErlError::GlobPattern(gperr) => write!(f, "Glob pattern error: {}", gperr),
      ErlError::Config(cfgerr) => write!(f, "Configuration file syntax error: {}", cfgerr),
      ErlError::PreprocessorParse { loc, msg } => {
        write!(f, "Preprocessor parse error: {} (at {})", msg, loc)
      }
      ErlError::PreprocessorSyntax { parse_err } => {
        write!(f, "Preprocessor syntax parse error: {}", parse_err)
      }
      ErlError::ParserInternal { loc, msg } => {
        write!(f, "Parser internal error: {} (at {})", msg, loc)
      }
      ErlError::ErlangParse { loc, msg } => {
        write!(f, "Erlang parse error: {} (at {})", msg, loc)
      }
      ErlError::ErlangSyntax { parse_err, msg } => {
        write!(f, "Erlang syntax parse error: {} - {}", parse_err, msg)
      }
      ErlError::TypeError(terr) => write!(f, "Type error: {}", terr),
    }
  }
}

impl ErlError {
  /// Some errors might result in a non-0 exit code, list them here
  pub fn process_exit_code(&self) -> i32 {
    // match self {}
    return 1;
  }

  /// Creates a preprocessor error from a filename and a message
  pub fn pp_parse<T>(file_name: &Path, message: &str) -> ErlResult<T> {
    Err(ErlError::PreprocessorParse {
      loc: ErrorLocation::new(Some(file_name.to_path_buf()),
                              SourceLoc::default()),
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
    let msg = value.to_string();
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
      loc: ErrorLocation::new(None, SourceLoc::default()),
      msg: format!("Cannot parse integer: {}", pie.to_string()),
    }
  }
}