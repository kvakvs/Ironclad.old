//! Access to sub-values in ErlAst

use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, ErlAstType};
use crate::erl_syntax::node::erl_binop::ErlBinaryOperatorExpr;
use crate::erl_syntax::node::erl_fn_def::ErlFnDef;
use crate::erl_syntax::preprocessor::ast::PreprocessorNodeType;
use crate::literal::Literal;
use crate::typing::erl_type::ErlType;
use std::ops::Deref;
use std::sync::Arc;

impl AstNodeImpl {
  /// Unwrap self as new function
  pub fn as_fn_def(&self) -> &ErlFnDef {
    match &self.content {
      ErlAstType::FnDef(func_def) => func_def,
      _ => panic!("Expected FnDef AST node, but got {}", self),
    }
  }

  /// Unwrap self as function spec
  pub fn as_fn_spec(&self) -> Arc<ErlType> {
    match &self.content {
      ErlAstType::FnSpec { spec, .. } => spec.clone(),
      _ => panic!("Expected FnSpec AST node, but got {}", self),
    }
  }

  /// Unwrap self as erltype
  pub fn as_type(&self) -> Arc<ErlType> {
    match &self.content {
      ErlAstType::Type { ty, .. } => ty.clone(),
      _ => panic!("Expected Type AST node, but got {}", self),
    }
  }

  /// Unwrap self as ironclad_exe operation expr
  pub fn as_binop(&self) -> &ErlBinaryOperatorExpr {
    match &self.content {
      ErlAstType::BinaryOp { expr, .. } => expr,
      _ => panic!("Expected BinOp AST node, but got {}", self),
    }
  }

  /// Unwrap self as an atom (return string slice or `panic`)
  pub fn as_atom(&self) -> &str {
    match &self.content {
      ErlAstType::Lit { value, .. } => match value.deref() {
        Literal::Atom(s) => s,
        _ => panic!("Expected Lit(Atom()) AST node, but got {}", self),
      },
      _ => panic!("Expected Lit(Atom()) AST node, but got {}", self),
    }
  }

  /// Unwrap a preprocessor node
  pub fn as_preprocessor(&self) -> &PreprocessorNodeType {
    match &self.content {
      ErlAstType::Preprocessor(pp) => pp,
      _ => panic!("Expected Preprocessor() AST node, but got {}", self),
    }
  }
}
