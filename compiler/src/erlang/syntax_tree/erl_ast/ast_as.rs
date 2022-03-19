//! Access to sub-values in ErlAst

use std::ops::Deref;
use std::sync::Arc;
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::node::erl_binop::ErlBinaryOperatorExpr;
use crate::erlang::syntax_tree::node::erl_fn_def::ErlFnDef;
use crate::literal::Literal;
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

  /// Unwrap self as erltype
  pub fn as_type(&self) -> Arc<ErlType> {
    match self {
      ErlAst::Type{ ty, .. } => ty.clone(),
      _ => panic!("Expected Type AST node, but got {}", self),
    }
  }

  /// Unwrap self as binary operation expr
  pub fn as_binop(&self) -> &ErlBinaryOperatorExpr {
    match self {
      ErlAst::BinaryOp{ expr, .. } => expr,
      _ => panic!("Expected BinOp AST node, but got {}", self),
    }
  }

  /// Unwrap self as an atom (return string slice or `panic`)
  pub fn as_atom(&self) -> &str {
    match self {
      ErlAst::Lit{ value, .. } => {
        match value.deref() {
          Literal::Atom(s) => s,
          _ => panic!("Expected Lit(Atom()) AST node, but got {}", self),
        }
      },
      _ => panic!("Expected Lit(Atom()) AST node, but got {}", self),
    }
  }
}
