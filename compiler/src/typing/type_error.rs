//! Type errors returned by the typing engine

use std::fmt::{Display, Formatter};
use crate::mfarity::MFArity;

/// Indicates various type problems
pub enum TypeError {
  /// Synthesized type for an expression isn't a subtype of the given type
  ExprNotASubtype {
    /// Type which is expected
    ty: String,
    /// Type to check: type synthesized from an expression
    expr_ty: String,
  },
  /// List operation received something that's not a list
  ListExpected {
    /// Message to go with the error
    msg: String
  },
  /// Returned when a call is attempted to something that's not a function
  NotAFunction {
    /// Message to go with the error
    msg: String,
  },
  /// Returned when function is not in function scope
  FunctionNotFound {
    /// The MFA that's not found
    mfa: MFArity,
  },
  /// A function call was attempted with wrong argument count
  BadArity {
    /// Message to go with the error
    msg: String,
  },
  /// A function call was attempted with incompatible arguments
  BadArguments {
    /// Message to go with the error
    msg: String,
  }
}

impl Display for TypeError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      TypeError::ExprNotASubtype { ty, expr_ty } =>
        write!(f, "Expression's type: {} but expected: {}", expr_ty, ty),
      TypeError::ListExpected { msg } => write!(f, "Bad list: {}", msg),
      TypeError::NotAFunction { msg } => write!(f, "Bad fun: {}", msg),
      TypeError::BadArity { msg } => write!(f, "Bad arity: {}", msg),
      TypeError::BadArguments { msg } => write!(f, "Bad arguments: {}", msg),
      TypeError::FunctionNotFound { mfa } => write!(f, "Function not found: {}", mfa),
    }
  }
}