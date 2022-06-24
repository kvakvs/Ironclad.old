//! Access to sub-values in ErlAst

use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_binop::ErlBinaryOperatorExpr;
use crate::erl_syntax::node::erl_fn_def::ErlFnDef;
use crate::literal::Literal;
use crate::typing::erl_type::ErlType;
use std::ops::Deref;

impl AstNodeImpl {
  /// Unwrap self as new function
  pub fn as_fn_def(&self) -> &ErlFnDef {
    match &self.content {
      AstNodeType::FnDef(func_def) => func_def,
      _ => panic!("Expected FnDef AST node, but got {}", self),
    }
  }

  /// Unwrap self as erltype
  pub fn as_type(&self) -> ErlType {
    match &self.content {
      AstNodeType::Type { ty, .. } => ty.clone(),
      _ => panic!("Expected Type AST node, but got {}", self),
    }
  }

  /// Unwrap self as binary operation expr
  #[allow(dead_code)]
  pub(crate) fn as_binop(&self) -> &ErlBinaryOperatorExpr {
    match &self.content {
      AstNodeType::BinaryOp { expr, .. } => expr,
      _ => panic!("Expected BinOp AST node, but got {}", self),
    }
  }

  /// Unwrap self as an atom (return string slice or `panic`)
  pub fn as_atom(&self) -> &str {
    match &self.content {
      AstNodeType::Lit { value, .. } => match value.deref() {
        Literal::Atom(s) => s,
        _ => panic!("Expected Lit(Atom()) AST node, but got {}", self),
      },
      _ => panic!("Expected Lit(Atom()) AST node, but got {}", self),
    }
  }

  /// Unwrap a `ModuleRoot` node. Returns name and child nodes vector. Returns children first
  /// level only, for flat list of everything use `.children()` call.
  #[allow(dead_code)]
  pub fn as_module(&self) -> &Vec<AstNode> {
    match &self.content {
      AstNodeType::ModuleRoot { forms } => forms,
      _ => panic!("Expected ModuleRoot() AST node, but got {}", self),
    }
  }
}
