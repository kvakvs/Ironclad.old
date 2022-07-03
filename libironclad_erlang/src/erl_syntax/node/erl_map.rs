//! Map datatype

use crate::erl_syntax::erl_ast::AstNode;
use std::fmt::Formatter;

/// Operation for a map member
#[derive(Debug, Clone)]
pub enum MapMemberOperation {
  /// Value goes into the map member: `Key => Value`
  Assign,
  /// Value from the map member matches and possibly binds in `MatchExpr`: `Key := MatchExpr`
  Match,
}

/// Defines a member of map construction AST node in `#{ key => value }`
#[derive(Debug, Clone)]
pub struct MapBuilderMember {
  pub operation: MapMemberOperation,
  /// The key to the map, an expression
  pub key: AstNode,
  /// The value to that key, an expression
  pub expr: AstNode,
}

impl MapBuilderMember {
  /// Create a new map member for assignment
  pub fn new_assign(key: AstNode, expr: AstNode) -> Self {
    MapBuilderMember { operation: MapMemberOperation::Assign, key, expr }
  }

  /// Create a new map member for a match operation
  pub fn new_match(key: AstNode, expr: AstNode) -> Self {
    MapBuilderMember { operation: MapMemberOperation::Match, key, expr }
  }
}

impl std::fmt::Display for MapBuilderMember {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{} => {}", self.key, self.expr)
  }
}
