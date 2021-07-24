//! Defines Application AST node for a function call
use std::rc::Rc;

use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::typing::erl_type::ErlType;

/// AST node which contains a function call
#[derive(PartialEq)]
pub struct Application {
  /// Target, to be called, expected to have function or lambda type fun((arg, arg,...) -> ret)
  pub expr: Rc<ErlAst>,
  /// Arguments. Their  inferred types are stored inside.
  pub args: Vec<Rc<ErlAst>>,
  /// Inferred type of return
  pub ret: ErlType,
}
