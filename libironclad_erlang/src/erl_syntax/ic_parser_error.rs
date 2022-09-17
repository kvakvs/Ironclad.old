//! Ironclad Error trait implementation for parser-produced errors

use crate::error::ic_error::IcSeverity;
use crate::error::ic_error_trait::IcErrorTrait;
use crate::source_loc::SourceLoc;
use std::fmt::{Display, Formatter};
use std::num::ParseIntError;

/// Errors produced by the parser
#[derive(Debug)]
pub struct IcParserError {
  /// The error severity
  pub severity: IcSeverity,
  /// Location where error was found
  pub location: SourceLoc,
  /// Message to display
  pub message: String,
}

impl IcParserError {
  /// Create a new parser error and box it
  pub fn new(severity: IcSeverity, location: SourceLoc, message: String) -> Box<Self> {
    Box::new(IcParserError { severity, location, message })
  }
}

impl Display for IcParserError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{} Parser {}: {}", self.location, self.severity, self.message)
  }
}

impl IcErrorTrait for IcParserError {
  fn get_severity(&self) -> IcSeverity {
    self.severity
  }

  fn get_location(&self) -> SourceLoc {
    self.location.clone()
  }

  fn get_process_exit_code(&self) -> i32 {
    3
  }

  fn get_message(&self) -> &str {
    &self.message
  }
}

impl From<ParseIntError> for IcParserError {
  fn from(pie: ParseIntError) -> Self {
    IcParserError {
      severity: IcSeverity::Error,
      location: SourceLoc::None,
      message: format!("Cannot parse integer: {}", pie),
    }
  }
}
