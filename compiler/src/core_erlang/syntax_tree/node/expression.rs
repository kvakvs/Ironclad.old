//! Defines structs for AST nodes representing binary operators (A + B) and unary (+A)
use std::sync::Arc;

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::core_erlang::syntax_tree::core_op::{CoreBinaryOp, CoreUnaryOp};

/// Binary operator is a code structure `Expr <operator> Expr`
#[derive(Debug)]
pub struct BinaryOperatorExpr {
  /// Left operand
  pub left: Arc<CoreAst>,
  /// Right operand
  pub right: Arc<CoreAst>,
  /// The operator
  pub operator: CoreBinaryOp,
}

impl BinaryOperatorExpr {
  // /// Gets the result type of a binary operation
  // pub fn get_result_type(&self) -> Arc<ErlType> {
  //   match self.operator {
  //     CoreBinaryOp::Add | CoreBinaryOp::Sub | CoreBinaryOp::Mul => {
  //       ErlType::union_of(vec![TypePrefab::any_integer(),
  //                              TypePrefab::float()], true)
  //     }
  //
  //     CoreBinaryOp::Div => TypePrefab::float(),
  //
  //     CoreBinaryOp::IntegerDiv => TypePrefab::any_integer(),
  //
  //     CoreBinaryOp::Modulo => TypePrefab::any_integer(),
  //
  //     CoreBinaryOp::Less | CoreBinaryOp::Greater | CoreBinaryOp::LessEq | CoreBinaryOp::GreaterEq
  //     | CoreBinaryOp::Eq | CoreBinaryOp::NotEq | CoreBinaryOp::HardEq | CoreBinaryOp::HardNotEq => {
  //       TypePrefab::any_bool()
  //     }
  //     CoreBinaryOp::ListAppend => {
  //       // Type of ++ will be union of left and right
  //       if let ErlType::List(left_list_t) = self.left.get_type().deref() {
  //         if let ErlType::List(right_list_t) = self.right.get_type().deref() {
  //           let union_t = ErlType::union_of(
  //             vec![left_list_t.clone(), right_list_t.clone()],
  //             true);
  //           return ErlType::List(union_t).into();
  //         } else {
  //           // right is not a list
  //         }
  //         // left is not a list
  //       }
  //       TypePrefab::none() // Raise TypeError::ListExpected?
  //     }
  //     CoreBinaryOp::ListSubtract => {
  //       // Type of -- will be left, probably some elements which should be missing, but how do we know?
  //       self.left.get_type()
  //     }
  //     CoreBinaryOp::Comma => self.right.get_type(),
  //   }
  // }

  // /// Gets the type for a binary operation, type is widened for numeric ops (return unions of
  // /// types) which later will be constrained by the type equations solver.
  // /// Returns None if the input type is not limited to any type.
  // pub fn get_arg_type(&self) -> Option<Arc<ErlType>> {
  //   match self.operator {
  //     CoreBinaryOp::Add | CoreBinaryOp::Sub | CoreBinaryOp::Mul | CoreBinaryOp::Div => {
  //       Some(ErlType::union_of(vec![TypePrefab::any_integer(), TypePrefab::float()],
  //                              true))
  //     }
  //
  //     CoreBinaryOp::IntegerDiv | CoreBinaryOp::Modulo => Some(TypePrefab::any_integer()),
  //
  //     CoreBinaryOp::Less | CoreBinaryOp::Greater | CoreBinaryOp::LessEq | CoreBinaryOp::GreaterEq
  //     | CoreBinaryOp::Eq | CoreBinaryOp::NotEq | CoreBinaryOp::HardEq | CoreBinaryOp::HardNotEq => {
  //       None
  //     }
  //
  //     CoreBinaryOp::ListAppend | CoreBinaryOp::ListSubtract => Some(TypePrefab::any_list()),
  //     CoreBinaryOp::Comma => Some(TypePrefab::any())
  //   }
  // }
}

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
