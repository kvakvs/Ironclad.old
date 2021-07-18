//! Collection of errors raised by the type analyzer and checker

use crate::typing::erl_type::ErlType;
use crate::typing::typevar::TypeVar;

/// Raised by the type analyzer and checker
#[derive(Debug)]
pub enum TypeError {
  /// Error raised when two types contradict one of type equations for the AST
  TypesDontMatch {
    /// First type not matching t2
    t1: ErlType,
    /// The second type not matching t1
    t2: ErlType,
  },
  /// Error raised when two functions of different arity are asserted to have equal types
  FunAritiesDontMatch,
  /// Error raised when a type is detected to be recursive
  OccursCheckFailed {
    /// The type variable which was checked
    tvar: TypeVar,
    /// The type which caused the error
    ty: ErlType,
  },
}