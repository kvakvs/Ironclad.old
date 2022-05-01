//! Creating ErlErrors from other types
use std::num::ParseIntError;
use crate::erl_error::{ErlError, ErlErrorType};
use crate::erlang::syntax_tree::nom_parse::ErlParserError;
use crate::source_loc::{SourceLoc};
use crate::typing::type_error::TypeError;

impl ErlError {
  /// Builds ErlError with nice error details from input string and Nom's verbose error
  pub fn from_nom_error(input: &str, value: ErlParserError) -> Self {
    Self {
      err_type: ErlErrorType::ErlangParse,
      loc: SourceLoc::None,
      msg: nom::error::convert_error(input, value),
    }
  }
}

impl From<std::io::Error> for ErlError {
  fn from(value: std::io::Error) -> Self {
    ErlError::new_type_only(ErlErrorType::Io(value))
  }
}

impl From<toml::de::Error> for ErlError {
  fn from(value: toml::de::Error) -> Self {
    ErlError::new_type_only(ErlErrorType::Config(value))
  }
}

impl From<glob::GlobError> for ErlError {
  fn from(value: glob::GlobError) -> Self {
    ErlError::new_type_only(ErlErrorType::Glob(value))
  }
}

impl From<glob::PatternError> for ErlError {
  fn from(value: glob::PatternError) -> Self {
    ErlError::new_type_only(ErlErrorType::GlobPattern(value))
  }
}

impl From<TypeError> for ErlError {
  fn from(value: TypeError) -> Self {
    ErlError::new_type_only(ErlErrorType::TypeErr(value))
  }
}

impl From<ParseIntError> for ErlError {
  fn from(pie: ParseIntError) -> Self {
    ErlError::new(ErlErrorType::ErlangParse,
                  SourceLoc::None,
                  format!("Cannot parse integer: {}", pie),
    )
  }
}
