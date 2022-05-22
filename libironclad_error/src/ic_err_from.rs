//! Creating ErlErrors from other types
use std::num::ParseIntError;
use crate::source_loc::SourceLoc;
use crate::ic_error::{IroncladError, };
use crate::ic_error_category::IcErrorCategory;

// impl IcError {
//   /// Builds IcError with nice error details from input string and Nom's verbose error
//   pub fn from_nom_error(input: &str, value: ErlParserError) -> Self {
//     Self {
//       err_type: IcErrorType::ErlangParse,
//       loc: SourceLoc::None,
//       msg: nom::error::convert_error(input, value),
//     }
//   }
// }

impl From<std::io::Error> for IroncladError {
  fn from(value: std::io::Error) -> Self {
    IroncladError::new_type_only(IcErrorCategory::Io(value))
  }
}

impl From<toml::de::Error> for IroncladError {
  fn from(value: toml::de::Error) -> Self {
    IroncladError::new_type_only(IcErrorCategory::Config(value))
  }
}

impl From<glob::GlobError> for IroncladError {
  fn from(value: glob::GlobError) -> Self {
    IroncladError::new_type_only(IcErrorCategory::Glob(value))
  }
}

impl From<glob::PatternError> for IroncladError {
  fn from(value: glob::PatternError) -> Self {
    IroncladError::new_type_only(IcErrorCategory::GlobPattern(value))
  }
}

// impl From<TypeError> for IcError {
//   fn from(value: TypeError) -> Self {
//     IcError::new_type_only(IcErrorCategory::TypeErr(value))
//   }
// }

impl From<ParseIntError> for IroncladError {
  fn from(pie: ParseIntError) -> Self {
    IroncladError::new(IcErrorCategory::ErlangParse,
                       SourceLoc::None,
                       format!("Cannot parse integer: {}", pie),
    )
  }
}
