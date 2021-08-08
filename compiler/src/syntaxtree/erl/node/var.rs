//! Defines struct for a variable AST node
use crate::typing::typevar::TypeVar;

/// AST node for a variable
#[derive(PartialEq)]
pub struct VarNode {
  /// Variable name
  pub name: String,
  /// Variable type for inference
  pub ty: TypeVar,
}

impl VarNode {
  /// Creates a new variable node
  pub fn new(name: &str) -> Self {
    VarNode {
      name: name.to_string(),
      ty: TypeVar::new(),
    }
  }
}
