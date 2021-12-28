//! Access to sub-values in ErlAst

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::node::erl_fn_def::ErlFnDef;

impl ErlAst {
  /// Unwrap self as new function
  pub fn as_fn_def(&self) -> &ErlFnDef {
    match self {
      ErlAst::FnDef(func_def) => func_def,
      _ => panic!("Expected FnDef AST node, but got {}", self),
    }
  }
}