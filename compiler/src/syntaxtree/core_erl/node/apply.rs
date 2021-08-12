//! Application (a function call on an expression)

use crate::syntaxtree::core_erl::core_ast::CoreAst;

/// Contains a function call
pub struct Apply {
  /// Must resolve to a callable
  pub target: Box<CoreAst>,
  /// Must match arity
  pub args: Vec<CoreAst>,
}
