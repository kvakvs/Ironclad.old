//! Contains is_* checks
use crate::literal::Literal;
use crate::typing::erl_type::ErlType;
use crate::typing::subtyping::SubtypeChecker;
use std::ops::Deref;

impl ErlType {
  /// Shortcut to the subtype checker
  #[inline]
  pub fn is_subtype_of(&self, other: &ErlType) -> bool {
    SubtypeChecker::is_subtype(self, other)
  }

  /// Checks whether type is an atom
  pub fn is_atom(&self) -> bool {
    return match self {
      ErlType::Atom | ErlType::Boolean => true,
      ErlType::Singleton { val } => {
        matches!(val.deref(), Literal::Atom(_))
      }
      _ => false,
    };
  }

  /// Checks whether type is a literal atom of value
  pub fn is_lit_atom(&self, s: &str) -> bool {
    return match self {
      ErlType::Singleton { val } => match val.deref() {
        Literal::Atom(actual) => actual == s,
        _ => false,
      },
      _ => false,
    };
  }

  /// Checks whether type is a number
  pub fn is_number(&self) -> bool {
    return match self {
      ErlType::Number | ErlType::Float | ErlType::Integer | ErlType::IntegerRange { .. } => true,
      ErlType::Singleton { val } => {
        matches!(val.deref(), Literal::Integer(_) | Literal::Float(_))
      }
      _ => false,
    };
  }

  /// Checks whether `self` is an integer or belongs to a complex type where integer is present too
  pub fn is_supertype_of_integer(&self) -> bool {
    self.is_integer() || ErlType::Integer.is_subtype_of(self)
  }

  /// Checks whether `self` is a float or belongs to a complex type where float is present too
  pub fn is_supertype_of_float(&self) -> bool {
    self.is_float() || ErlType::Float.is_subtype_of(self)
  }

  /// Checks whether `self` is an integer or a float, or belongs to a complex type where integer or
  /// float are present.
  pub fn is_supertype_of_number(&self) -> bool {
    self.is_number() || ErlType::Number.is_subtype_of(self)
  }

  /// Checks whether type is an integer number
  pub fn is_integer(&self) -> bool {
    return match self {
      ErlType::Integer | ErlType::IntegerRange { .. } => true,
      ErlType::Singleton { val } => {
        matches!(val.deref(), Literal::Integer(_))
      }
      _ => false,
    };
  }

  /// Checks whether type is a floating point number (or an integer, because compatible why not)
  pub fn is_float(&self) -> bool {
    return match self {
      ErlType::Float => true,
      ErlType::Singleton { val } => {
        matches!(val.deref(), Literal::Float(_)) // | Literal::Integer(_) | Literal::BigInteger
      }
      _ => false,
    };
  }

  /// Checks whether type is a tuple type
  pub fn is_tuple(&self) -> bool {
    matches!(self, ErlType::AnyTuple | ErlType::Tuple { .. } | ErlType::IntegerRange { .. })
  }

  /// Checks whether type is an union type
  pub fn is_union(&self) -> bool {
    matches!(self, ErlType::Union { .. })
  }

  /// Checks whether type is a list
  pub fn is_list(&self) -> bool {
    return match self {
      ErlType::AnyList
      | ErlType::List { .. }
      | ErlType::StronglyTypedList { .. }
      | ErlType::Nil => true,
      ErlType::Singleton { val: singleton } => {
        matches!(singleton.deref(), Literal::List { .. } | Literal::String { .. })
      }
      _ => false,
    };
  }

  /// Checks whether type is an empty list (NIL)
  pub fn is_nil(&self) -> bool {
    matches!(self, ErlType::Nil)
  }

  /// Checks whether type is a ironclad_exe
  pub fn is_binary(&self) -> bool {
    matches!(self, ErlType::AnyBinary | ErlType::Binary { .. })
  }

  /// Checks whether type is a map
  pub fn is_map(&self) -> bool {
    matches!(self, ErlType::AnyMap | ErlType::Map { .. })
  }

  /// Checks whether a type is callable
  pub fn is_function(&self) -> bool {
    matches!(
      self,
      ErlType::AnyFn | ErlType::Fn { .. } | ErlType::FnRef { .. } | ErlType::Lambda
    )
  }

  /// True if type is any()
  pub fn is_any(&self) -> bool {
    matches!(self, ErlType::Any)
  }

  /// True if type is none() or union of no types
  pub fn is_none(&self) -> bool {
    match self {
      ErlType::None => true,
      ErlType::Union(tu) if tu.is_empty() => true,
      _ => false,
    }
  }
}
