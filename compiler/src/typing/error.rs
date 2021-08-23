//! Collection of errors raised by the type analyzer and checker

use crate::typing::erl_type::ErlType;
use crate::typing::typevar::TypeVar;
use std::fmt::Formatter;
use crate::mfarity::MFArity;
use std::sync::Arc;

/// Raised by the type analyzer and checker
pub enum TypeError {
  /// Error raised when two types contradict one of type equations for the AST
  TypesDontMatch {
    /// First type not matching t2
    t1: Arc<ErlType>,
    /// The second type not matching t1
    t2: Arc<ErlType>,
  },
  /// Error raised when two functions of different arity are asserted to have equal types
  FunAritiesDontMatch,
  /// Error raised when a type is detected to be recursive
  OccursCheckFailed {
    /// The type variable which was checked
    tvar: TypeVar,
    /// The type which caused the error
    ty: Arc<ErlType>,
  },
  // /// Error when operation wanted a list, and received 'received'
  // ListExpected { received: ErlType },
  /// Local function in a module is not found
  LocalFunctionUndef {
    /// The module name
    module: String,
    /// The function name and arity
    funarity: MFArity,
  },
}

impl std::fmt::Display for TypeError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      TypeError::TypesDontMatch { t1, t2 } => {
        write!(f, "Types don't match: {} <=> {}", t1, t2)
      }
      TypeError::FunAritiesDontMatch => write!(f, "Function arities don't match"),
      TypeError::OccursCheckFailed { tvar, ty } => {
        write!(f, "Recursive type {} in {}", tvar, ty)
      }
      TypeError::LocalFunctionUndef { module, funarity } => {
        write!(f, "Local function {} not found in module {}", funarity, module)
      }
    }
  }
}