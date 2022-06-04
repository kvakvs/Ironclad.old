//! Map datatype

use crate::erl_syntax::erl_ast::AstNode;
use std::fmt::Formatter;

/// Defines a member of map construction AST node in `#{ key => value }`
#[derive(Debug)]
pub struct MapBuilderMember {
  /// The key to the map, an expression
  pub key: AstNode,
  /// The value to that key, an expression
  pub value: AstNode,
}

impl std::fmt::Display for MapBuilderMember {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{} => {}", self.key, self.value)
  }
}
