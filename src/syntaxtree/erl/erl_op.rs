use crate::typing::erltype::Type;
use crate::erl_error::{ErlResult, ErlError};
use crate::typing::erltype::TypeError::MathOpNumberExpected;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ErlBinaryOp {
  Add,
  Sub,
  Mul,
  Div,
  IntDiv,
  Modulo,
  Less,
  Greater,
  LessEq,
  GreaterEq,
  Eq,
  NotEq,
}

impl ErlBinaryOp {
  /// Get full type for the binary op in form of (arg1_type -> arg2_type -> result_type)
  pub fn op_type(op: ErlBinaryOp, t1: &Type, t2: &Type) -> ErlResult<Type> {
    // TODO: Can check t1 and t2 for add/sub/mul/div and return float if a float is involved
    match op {
      ErlBinaryOp::Add => Self::math_binop_type(op, t1, t2),
      ErlBinaryOp::Sub => Self::math_binop_type(op, t1, t2),
      ErlBinaryOp::Mul => Self::math_binop_type(op, t1, t2),
      ErlBinaryOp::Div => Self::math_binop_type(op, t1, t2),
      ErlBinaryOp::IntDiv => Ok(Type::new_fun(&Type::integer_vec2(), Type::integer())),
      ErlBinaryOp::Modulo => Ok(Type::new_fun(&Type::integer_vec2(), Type::integer())),
      ErlBinaryOp::Less => Ok(Type::new_fun(&Type::integer_vec2(), Type::bool())), // TODO: any term
      ErlBinaryOp::Greater => Ok(Type::new_fun(&Type::integer_vec2(), Type::bool())), // TODO: any term
      ErlBinaryOp::LessEq => Ok(Type::new_fun(&Type::integer_vec2(), Type::bool())), // TODO: any term
      ErlBinaryOp::GreaterEq => Ok(Type::new_fun(&Type::integer_vec2(), Type::bool())), // TODO: any term
      ErlBinaryOp::Eq => Ok(Type::new_fun(&Type::integer_vec2(), Type::bool())), // TODO: any term
      ErlBinaryOp::NotEq => Ok(Type::new_fun(&Type::integer_vec2(), Type::bool())), // TODO: any term
    }
  }

  fn math_binop_type(op: ErlBinaryOp, t1: &Type, t2: &Type) -> ErlResult<Type> {
    if t1 != Type::integer() || t1 != Type::float() {
      return Err(ErlError::TypeError(MathOpNumberExpected { arg: 1, op }));
    }
    if t2 != Type::integer() || t2 != Type::float() {
      return Err(ErlError::TypeError(MathOpNumberExpected { arg: 2, op }));
    }
    let ret_type = if op == ErlBinaryOp::Div {
      Type::float() // Division result is always float
    } else if t1 == Type::float() || t2 == Type::float() {
      Type::float() // Add/Sub/Mul result is float if one of argument is float
    } else {
      Type::integer() // Two integer arguments produce integer result
    };
    let result = Type::new_fun(&vec![t1.clone(), t2.clone()], ret_type);
    Ok(result)
  }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ErlUnaryOp {
  Not,
  Negate,
}
