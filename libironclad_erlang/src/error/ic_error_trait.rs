//! Trait for Ironclad errors

use crate::error::ic_error::{IcSeverity, IroncladError};
use crate::error::ic_error_kind::IcErrorKind;
use crate::source_loc::SourceLoc;

/// Trait for Ironclad errors, allows grouping errors into a big `IcError` struct
pub trait IcErrorTrait: std::fmt::Display + std::fmt::Debug {
  /// Generalized category for the error, not aware of how sub-libraries are handling their errors
  fn get_severity(&self) -> IcSeverity;

  /// Where the error occured
  fn get_location(&self) -> SourceLoc;

  /// Some errors might result in a non-0 exit code, return it here
  fn get_process_exit_code(&self) -> i32;

  /// Retrieve the text message to the user
  fn get_message(&self) -> &str;
}

/// A boxed dynamic pointer to an unknown error, implementor of `IcErrorT` trait
pub type GenericIroncladError = Box<dyn IcErrorTrait>;

impl From<IroncladError> for GenericIroncladError {
  fn from(ironclad_err: IroncladError) -> GenericIroncladError {
    Box::new(ironclad_err)
  }
}
