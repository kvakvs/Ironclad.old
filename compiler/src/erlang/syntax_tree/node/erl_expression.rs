//! Defines structs for AST nodes representing binary operators (A + B) and unary (+A)
use std::sync::Arc;

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::erl_op::{ErlBinaryOp, ErlUnaryOp};
use crate::typing::erl_type::ErlType;

/// Binary operator is a code structure `Expr <operator> Expr`
#[derive(Debug)]
pub struct ErlBinaryOperatorExpr {
  /// Left operand
  pub left: Arc<ErlAst>,
  /// Right operand
  pub right: Arc<ErlAst>,
  /// The operator
  pub operator: ErlBinaryOp,
}

/// Unary operator is right-associative operation such as `not A` or `+A`
#[derive(Debug)]
pub struct ErlUnaryOperatorExpr {
  /// The operand
  pub expr: Arc<ErlAst>,
  /// The operator
  pub operator: ErlUnaryOp,
}

impl ErlUnaryOperatorExpr {
  /// Get the type of an unary operation. Input type is same as return type.
  pub fn synthesize_type(&self) -> Arc<ErlType> {
    match self.operator {
      ErlUnaryOp::Not => ErlType::boolean(),

      ErlUnaryOp::Negative
      | ErlUnaryOp::Positive => ErlType::number(),

      ErlUnaryOp::Catch => ErlType::any(),
    }
  }
}
