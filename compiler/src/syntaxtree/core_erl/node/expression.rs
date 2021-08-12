//! Defines structs for AST nodes representing binary operators (A + B) and unary (+A)
use crate::typing::erl_type::ErlType;
use crate::typing::typevar::TypeVar;
use crate::syntaxtree::core_erl::core_ast::CoreAst;
use crate::syntaxtree::core_erl::core_op::{CoreBinaryOp, CoreUnaryOp};

/// Binary operator is a code structure `Expr <operator> Expr`
// #[derive(PartialEq)]
pub struct BinaryOperatorExpr {
  /// Left operand
  pub left: Box<CoreAst>,
  /// Right operand
  pub right: Box<CoreAst>,
  /// The operator
  pub operator: CoreBinaryOp,
  /// The return type of the operation
  pub ty: TypeVar,
}

impl BinaryOperatorExpr {
  /// Gets the result type of a binary operation
  pub fn get_result_type(&self) -> ErlType {
    match self.operator {
      CoreBinaryOp::Add | CoreBinaryOp::Sub | CoreBinaryOp::Mul => {
        ErlType::union_of(vec![ErlType::AnyInteger, ErlType::Float], true)
      }

      | CoreBinaryOp::Div => ErlType::Float,

      CoreBinaryOp::IntegerDiv => ErlType::AnyInteger,

      CoreBinaryOp::Modulo => ErlType::AnyInteger,

      CoreBinaryOp::Less | CoreBinaryOp::Greater | CoreBinaryOp::LessEq | CoreBinaryOp::GreaterEq
      | CoreBinaryOp::Eq | CoreBinaryOp::NotEq | CoreBinaryOp::HardEq | CoreBinaryOp::HardNotEq => {
        ErlType::AnyBool
      }
      CoreBinaryOp::ListAppend => {
        // Type of ++ will be union of left and right
        if let ErlType::List(left_list_t) = self.left.get_type() {
          if let ErlType::List(right_list_t) = self.right.get_type() {
            let union_t = ErlType::union_of(vec![*left_list_t, *right_list_t], true);
            return ErlType::List(Box::new(union_t));
          } else {
            // right is not a list
          }
          // left is not a list
        }
        ErlType::None // Raise TypeError::ListExpected?
      }
      CoreBinaryOp::ListSubtract => {
        // Type of -- will be left, probably some elements which should be missing, but how do we know?
        self.left.get_type()
      }
      CoreBinaryOp::Comma => self.right.get_type(),
    }
  }

  /// Gets the type for a binary operation, type is widened for numeric ops (return unions of
  /// types) which later will be constrained by the type equations solver.
  /// Returns None if the input type is not limited to any type.
  pub fn get_arg_type(&self) -> Option<ErlType> {
    match self.operator {
      CoreBinaryOp::Add | CoreBinaryOp::Sub | CoreBinaryOp::Mul | CoreBinaryOp::Div => {
        Some(ErlType::union_of(vec![ErlType::AnyInteger, ErlType::Float], true))
      }

      CoreBinaryOp::IntegerDiv | CoreBinaryOp::Modulo => {
        Some(ErlType::AnyInteger)
      }

      CoreBinaryOp::Less | CoreBinaryOp::Greater | CoreBinaryOp::LessEq | CoreBinaryOp::GreaterEq
      | CoreBinaryOp::Eq | CoreBinaryOp::NotEq | CoreBinaryOp::HardEq | CoreBinaryOp::HardNotEq => {
        None
      }

      CoreBinaryOp::ListAppend | CoreBinaryOp::ListSubtract => Some(ErlType::AnyList),
      CoreBinaryOp::Comma => Some(ErlType::Any)
    }
  }
}

/// Unary operator is right-associative operation such as `not A` or `+A`
// #[derive(PartialEq)]
pub struct UnaryOperatorExpr {
  /// The operand
  pub expr: Box<CoreAst>,
  /// The operator
  pub operator: CoreUnaryOp,
}

impl UnaryOperatorExpr {
  /// Get the type of an unary operation. Input type is same as return type.
  pub fn get_type(&self) -> ErlType {
    match self.operator {
      CoreUnaryOp::Not => ErlType::AnyBool,

      CoreUnaryOp::Negative
      | CoreUnaryOp::Positive => {
        ErlType::union_of(vec![ErlType::AnyInteger, ErlType::Float], true)
      }

      CoreUnaryOp::Catch => ErlType::Any,
    }
  }
}
