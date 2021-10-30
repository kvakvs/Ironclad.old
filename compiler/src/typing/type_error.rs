//! Type errors returned by the typing engine

use std::fmt::{Display, Formatter};

/// Indicates various type problems
pub enum TypeError {
  /// Dummy
  None,
}

impl Display for TypeError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      TypeError::None => write!(f, "No type error, all is good"),
    }
  }
}