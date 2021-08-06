//! Defines structs for AST nodes representing binary operators (A + B) and unary (+A)
use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::syntaxtree::erl::erl_op::{ErlBinaryOp, ErlUnaryOp};
use crate::typing::erl_type::ErlType;

/// Binary operator is a code structure `Expr <operator> Expr`
// #[derive(PartialEq)]
pub struct BinaryOperatorExprNode {
  /// Left operand
  pub left: Box<ErlAst>,
  /// Right operand
  pub right: Box<ErlAst>,
  /// The operator
  pub operator: ErlBinaryOp,
  /// The return type of the operation
  pub ty: ErlType,
}

impl BinaryOperatorExprNode {
  /// Gets the result type of a binary operation
  pub fn get_result_type(&self) -> ErlType {
    match self.operator {
      ErlBinaryOp::Add | ErlBinaryOp::Sub | ErlBinaryOp::Mul => {
        ErlType::union_of(vec![ErlType::AnyInteger, ErlType::Float])
      }

      | ErlBinaryOp::Div => ErlType::Float,

      ErlBinaryOp::IntegerDiv => ErlType::AnyInteger,

      ErlBinaryOp::Modulo => ErlType::AnyInteger,

      ErlBinaryOp::Less | ErlBinaryOp::Greater | ErlBinaryOp::LessEq | ErlBinaryOp::GreaterEq
      | ErlBinaryOp::Eq | ErlBinaryOp::NotEq | ErlBinaryOp::HardEq | ErlBinaryOp::HardNotEq => {
        ErlType::AnyBool
      }
      ErlBinaryOp::ListAppend => {
        // Type of ++ will be union of left and right
        if let ErlType::List(left_list_t) = self.left.get_type() {
          if let ErlType::List(right_list_t) = self.right.get_type() {
            let union_t = ErlType::union_of(vec![*left_list_t, *right_list_t]);
            return ErlType::List(Box::new(union_t));
          } else {
            // right is not a list
          }
          // left is not a list
        }
        ErlType::None // Raise TypeError::ListExpected?
      }
      ErlBinaryOp::ListSubtract => {
        // Type of -- will be left, probably some elements which should be missing, but how do we know?
        self.left.get_type()
      }
    }
  }

  /// Gets the type for a binary operation, type is widened for numeric ops (return unions of
  /// types) which later will be constrained by the type equations solver.
  /// Returns None if the input type is not limited to any type.
  pub fn get_arg_type(&self) -> Option<ErlType> {
    match self.operator {
      ErlBinaryOp::Add | ErlBinaryOp::Sub | ErlBinaryOp::Mul | ErlBinaryOp::Div => {
        Some(ErlType::union_of(vec![ErlType::AnyInteger, ErlType::Float]))
      }

      ErlBinaryOp::IntegerDiv | ErlBinaryOp::Modulo => {
        Some(ErlType::AnyInteger)
      }

      ErlBinaryOp::Less | ErlBinaryOp::Greater | ErlBinaryOp::LessEq | ErlBinaryOp::GreaterEq
      | ErlBinaryOp::Eq | ErlBinaryOp::NotEq | ErlBinaryOp::HardEq | ErlBinaryOp::HardNotEq => {
        None
      }

      ErlBinaryOp::ListAppend | ErlBinaryOp::ListSubtract => Some(ErlType::AnyList),
    }
  }
}

/// Unary operator is right-associative operation such as `not A` or `+A`
// #[derive(PartialEq)]
pub struct UnaryOperatorExprNode {
  /// The operand
  pub expr: Box<ErlAst>,
  /// The operator
  pub operator: ErlUnaryOp,
}

impl UnaryOperatorExprNode {
  /// Get the type of an unary operation. Input type is same as return type.
  pub fn get_type(&self) -> ErlType {
    match self.operator {
      ErlUnaryOp::Not => ErlType::AnyBool,

      ErlUnaryOp::Negative
      | ErlUnaryOp::Positive => {
        ErlType::union_of(vec![ErlType::AnyInteger, ErlType::Float])
      }

      ErlUnaryOp::Catch => ErlType::Any,
    }
  }
}
