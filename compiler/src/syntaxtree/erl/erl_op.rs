//! Binary and unary operations used in type checking.
use crate::typing::erl_type::ErlType;
use crate::typing::erl_type::ErlType::{Bool, Float, Integer};

/// Binary operation taking two arguments
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ErlBinaryOp {
  /// Sum of two any numbers
  Add,
  /// Subtraction resut of two any numbers
  Sub,
  /// Product of two any numbers
  Mul,
  /// Float division result of two any numbers
  Div,
  /// Integer division result of two integer numbers
  IntegerDiv,
  /// Division remainder of two integer numbers
  Modulo,
  /// Left value is less than the right value
  Less,
  /// Left value is greater than the right value
  Greater,
  /// Left value is less or equal to the right value
  LessEq,
  /// Left value is greater or equal to the right value
  GreaterEq,
  /// Equality of two any values
  Eq,
  /// Inequality of two any values
  NotEq,
}

impl ErlBinaryOp {
  /// Gets the type for a binary operation, type is widened for numeric ops (return unions of
  /// types) which later will be constrained by the type equations solver.
  /// Returns None if the input type is not limited to any type.
  pub fn get_arg_type(&self) -> Option<ErlType> {
    match self {
      ErlBinaryOp::Add
      | ErlBinaryOp::Sub
      | ErlBinaryOp::Mul
      | ErlBinaryOp::Div => Some(ErlType::new_union(vec![ErlType::Integer, ErlType::Float])),

      ErlBinaryOp::IntegerDiv
      | ErlBinaryOp::Modulo => Some(ErlType::Integer),

      ErlBinaryOp::Less
      | ErlBinaryOp::Greater
      | ErlBinaryOp::LessEq
      | ErlBinaryOp::GreaterEq
      | ErlBinaryOp::Eq
      | ErlBinaryOp::NotEq => None
    }
  }

  /// Gets the result type of a binary operation
  pub fn get_result_type(&self) -> ErlType {
    match self {
      ErlBinaryOp::Add
      | ErlBinaryOp::Sub
      | ErlBinaryOp::Mul=> ErlType::new_union(vec![Integer, Float]),
      | ErlBinaryOp::Div => Float,
      ErlBinaryOp::IntegerDiv => Integer,
      ErlBinaryOp::Modulo => Integer,
      ErlBinaryOp::Less
      | ErlBinaryOp::Greater
      | ErlBinaryOp::LessEq
      | ErlBinaryOp::GreaterEq
      | ErlBinaryOp::Eq
      | ErlBinaryOp::NotEq => Bool
    }
  }
}

/// Unary operation takes 1 argument of bool or number, and returns same type
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ErlUnaryOp {
  /// Logical negation
  Not,
  /// Numerical sign change
  Negate,
}

impl ErlUnaryOp {
  /// Get the type of an unary operation. Input type is same as return type.
  pub fn get_type(&self) -> ErlType {
    match self {
      ErlUnaryOp::Not => ErlType::Bool,
      ErlUnaryOp::Negate => ErlType::new_union(vec![Integer, Float]),
    }
  }
}
