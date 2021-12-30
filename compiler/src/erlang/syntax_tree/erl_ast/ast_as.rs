//! Access to sub-values in ErlAst

use std::sync::Arc;
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::node::erl_fn_def::ErlFnDef;
use crate::typing::erl_type::ErlType;

impl ErlAst {
  /// Unwrap self as new function
  pub fn as_fn_def(&self) -> &ErlFnDef {
    match self {
      ErlAst::FnDef(func_def) => func_def,
      _ => panic!("Expected FnDef AST node, but got {}", self),
    }
  }

  /// Unwrap self as function spec
  pub fn as_fn_spec(&self) -> Arc<ErlType> {
    match self {
      ErlAst::FnSpec{ spec, .. } => spec.clone(),
      _ => panic!("Expected FnSpec AST node, but got {}", self),
    }
  }
}