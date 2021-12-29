//! Creating ErlErrors from other types
use std::num::ParseIntError;
use crate::erl_error::ErlError;
use crate::preprocessor::syntax_tree::pp_parser;
use crate::source_loc::{ErrorLocation, SourceLoc};
use crate::typing::type_error::TypeError;

impl From<nom::Err<nom::error::Error<&str>>> for ErlError {
  fn from(value: nom::Err<nom::error::Error<&str>>) -> Self {
    ErlError::ErlangParse {
      loc: ErrorLocation::empty(),
      msg: format!("{}", value),
    }
  }
}

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

// impl From<pest::error::Error<erl_parser_prec_climber::Rule>> for ErlError {
//   fn from(value: pest::error::Error<erl_parser_prec_climber::Rule>) -> Self {
//     let msg = value.to_string();
//     ErlError::ErlangSyntax {
//       parse_err: value,
//       msg,
//     }
//   }
// }

impl From<TypeError> for ErlError {
  fn from(value: TypeError) -> Self {
    ErlError::TypeErr(value)
  }
}

impl From<ParseIntError> for ErlError {
  fn from(pie: ParseIntError) -> Self {
    ErlError::ErlangParse {
      loc: ErrorLocation::new(None, SourceLoc::None),
      msg: format!("Cannot parse integer: {}", pie.to_string()),
    }
  }
}
