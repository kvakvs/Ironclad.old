//! Map datatype

use crate::erl_syntax::erl_ast::ErlAst;
use std::fmt::Formatter;
use std::sync::Arc;

/// Defines a member of map construction AST node in `#{ key => value }`
#[derive(Debug)]
pub struct MapBuilderMember {
  /// The key to the map, an expression
  pub key: Arc<ErlAst>,
  /// The value to that key, an expression
  pub value: Arc<ErlAst>,
}

impl std::fmt::Display for MapBuilderMember {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{} => {}", self.key, self.value)
  }
}
