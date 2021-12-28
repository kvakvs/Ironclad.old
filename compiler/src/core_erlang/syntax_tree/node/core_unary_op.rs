//! Defines structs for AST nodes representing unary operators (+A, not A, -A, ...)
#![cfg(coreast)]

use std::sync::Arc;
use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::core_erlang::syntax_tree::core_op::CoreUnaryOp;

/// Unary operator is right-associative operation such as `not A` or `+A`
#[derive(Debug)]
pub struct UnaryOperatorExpr {
  /// The operand
  pub expr: Arc<CoreAst>,
  /// The operator
  pub operator: CoreUnaryOp,
}

impl UnaryOperatorExpr {
  // /// Get the type of an unary operation. Input type is same as return type.
  // pub fn get_type(&self) -> Arc<ErlType> {
  //   match self.operator {
  //     CoreUnaryOp::Not => TypePrefab::any_bool(),
  //
  //     CoreUnaryOp::Negative
  //     | CoreUnaryOp::Positive => {
  //       ErlType::union_of(vec![TypePrefab::any_integer(), TypePrefab::float()], true)
  //     }
  //
  //     CoreUnaryOp::Catch => TypePrefab::any(),
  //   }
  // }
}
