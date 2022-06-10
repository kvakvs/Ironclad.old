//! Defines struct for a variable AST node

use derivative::Derivative;

/// AST node for a variable
#[derive(Clone, Derivative)]
#[derivative(PartialEq, Debug)]
pub struct ErlVar {
  /// Variable name
  pub name: String,
}

impl ErlVar {
  /// Creates a new variable node
  pub(crate) fn new(name: &str) -> Self {
    ErlVar { name: name.to_string() }
  }
}
