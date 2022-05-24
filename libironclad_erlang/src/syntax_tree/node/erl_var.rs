//! Defines struct for a variable AST node

use derivative::Derivative;
use libironclad_error::source_loc::SourceLoc;

/// AST node for a variable
#[derive(Clone, Derivative)]
#[derivative(PartialEq, Debug)]
pub struct ErlVar {
  /// Source code pointer
  #[derivative(PartialEq = "ignore")]
  #[derivative(Debug = "ignore")]
  pub location: SourceLoc,

  /// Variable name
  pub name: String,
}

impl ErlVar {
  /// Creates a new variable node
  pub fn new(location: SourceLoc, name: &str) -> Self {
    ErlVar { location, name: name.to_string() }
  }
}
