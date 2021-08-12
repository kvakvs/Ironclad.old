//! Application (a function call on an expression)

use crate::syntaxtree::core_erl::core_ast::CoreAst;
use crate::typing::typevar::TypeVar;
use std::fmt::Formatter;
use crate::display;

/// Contains a function call
pub struct Apply {
  /// Must resolve to a callable
  pub target: Box<CoreAst>,
  /// Must match arity
  pub args: Vec<CoreAst>,
  /// The unique typevar for return type
  pub ret_ty: TypeVar,
}

impl std::fmt::Display for Apply {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "apply {} ", self.target)?;
    display::display_paren_list(&self.args, f)
  }
}
