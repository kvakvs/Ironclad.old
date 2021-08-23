//! Defines struct for a variable AST node
use crate::source_loc::SourceLoc;

/// AST node for a variable
#[derive(Derivative)]
#[derivative(PartialEq, Debug)]
pub struct ErlVar {
  /// Source code pointer
  #[derivative(PartialEq="ignore")]
  #[derivative(Debug="ignore")]
  pub location: SourceLoc,

  /// Variable name
  pub name: String,
}

impl ErlVar {
  /// Creates a new variable node
  pub fn new(location: SourceLoc, name: &str) -> Self {
    ErlVar {
      location,
      name: name.to_string(),
    }
  }
}
