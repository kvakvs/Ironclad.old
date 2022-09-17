//! Creating ErlErrors from other types
use crate::error::ic_error::{IcSeverity, IroncladError};
use crate::error::ic_error_kind::IcErrorKind;
use crate::source_loc::SourceLoc;
use libironclad_util::io::file_error::IcFileError;
use std::num::ParseIntError;

impl From<std::io::Error> for IroncladError {
  fn from(value: std::io::Error) -> Self {
    IroncladError::new_type_only(IcSeverity::Error, IcErrorKind::StdIoError(value))
  }
}

impl From<IcFileError> for IroncladError {
  fn from(value: IcFileError) -> Self {
    IroncladError::new_type_only(IcSeverity::Error, IcErrorKind::IcFileError(value))
  }
}

impl From<toml::de::Error> for IroncladError {
  fn from(value: toml::de::Error) -> Self {
    IroncladError::new_type_only(IcSeverity::Error, IcErrorKind::Config(value))
  }
}

impl From<glob::GlobError> for IroncladError {
  fn from(value: glob::GlobError) -> Self {
    IroncladError::new_type_only(IcSeverity::Error, IcErrorKind::Glob(value))
  }
}

impl From<glob::PatternError> for IroncladError {
  fn from(value: glob::PatternError) -> Self {
    IroncladError::new_type_only(IcSeverity::Error, IcErrorKind::GlobPattern(value))
  }
}

// impl From<ParseIntError> for IroncladError {
//   fn from(pie: ParseIntError) -> Self {
//     IroncladError::new(
//       IcSeverity::Error,
//       IcErrorKind::ErlangParse,
//       SourceLoc::None,
//       format!("Cannot parse integer: {}", pie),
//     )
//   }
// }
