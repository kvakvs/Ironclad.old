//! Error enum for file IO operations

use std::fmt::{Display, Formatter};

/// Wraps file IO errors for util library
#[derive(Debug)]
pub enum IcFileError {
  StdIoError(std::io::Error),
}

impl From<std::io::Error> for IcFileError {
  fn from(e: std::io::Error) -> Self {
    Self::StdIoError(e)
  }
}

impl Display for IcFileError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::StdIoError(e) => e.fmt(f),
    }
  }
}
