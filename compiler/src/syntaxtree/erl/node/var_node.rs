//! Defines struct for a variable AST node
use crate::typing::erl_type::ErlType;

/// AST node for a variable
#[derive(PartialEq)]
pub struct VarNode {
  /// Variable name
  pub name: String,
  /// Variable type for inference
  pub ty: ErlType,
}

impl VarNode {
  /// Creates a new variable node
  pub fn new(name: &str) -> Self {
    VarNode {
      name: name.to_string(),
      ty: ErlType::new_typevar(),
    }
  }
}
