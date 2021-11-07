//! Type errors returned by the typing engine

use std::fmt::{Display, Formatter};

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
}

impl Display for TypeError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      TypeError::ExprNotASubtype { ty, expr_ty } =>
        write!(f, "Expression's type: {} but expected: {}", expr_ty, ty),
      TypeError::ListExpected { msg } => write!(f, "{}", msg),
    }
  }
}