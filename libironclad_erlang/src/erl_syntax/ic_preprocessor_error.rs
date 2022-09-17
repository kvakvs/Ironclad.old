//! Ironclad Error trait implementation for parser-produced errors

use crate::error::ic_error::IcSeverity;
use crate::error::ic_error_trait::IcErrorTrait;
use crate::source_loc::SourceLoc;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub struct IcPreprocessorError {
  pub severity: IcSeverity,
  /// Location where error was found
  pub location: SourceLoc,
  /// Message to display
  pub message: String,
}

impl IcPreprocessorError {
  pub fn new(severity: IcSeverity, location: SourceLoc, message: String) -> Box<Self> {
    Box::new(IcPreprocessorError { severity, location, message })
  }
}

impl Display for IcPreprocessorError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{} Preprocessor {}: {}", self.location, self.severity, self.message)
  }
}
impl IcErrorTrait for IcPreprocessorError {
  fn get_severity(&self) -> IcSeverity {
    self.severity
  }

  fn get_location(&self) -> SourceLoc {
    self.location.clone()
  }

  fn get_process_exit_code(&self) -> i32 {
    2
  }

  fn get_message(&self) -> &str {
    &self.message
  }
}
