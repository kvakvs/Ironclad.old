//! Contains is_* checks
use crate::literal::Literal;
use crate::typing::erl_type::typekind::TypeKind;
use crate::typing::erl_type::TypeImpl;
use crate::typing::subtyping::SubtypeChecker;
use std::ops::Deref;

impl TypeImpl {
  /// Shortcut to the subtype checker
  pub fn is_subtype_of(&self, other: &TypeImpl) -> bool {
    SubtypeChecker::is_subtype(self, other)
  }

  /// Checks whether type is an atom
  pub fn is_atom(&self) -> bool {
    match &self.kind {
      TypeKind::Atom | TypeKind::Boolean => true,
      TypeKind::Singleton { val } => {
        matches!(val.deref(), Literal::Atom(_))
      }
      _ => false,
    }
  }

  /// Checks whether type is a literal atom of value
  pub fn is_lit_atom(&self, s: &str) -> bool {
    return match &self.kind {
      TypeKind::Singleton { val } => match val.deref() {
        Literal::Atom(actual) => actual == s,
        _ => false,
      },
      _ => false,
    };
  }

  /// Checks whether type is a number
  #[allow(dead_code)]
  pub(crate) fn is_number(&self) -> bool {
    match &self.kind {
      TypeKind::Number | TypeKind::Float | TypeKind::Integer | TypeKind::IntegerRange { .. } => {
        true
      }
      TypeKind::Singleton { val } => {
        matches!(val.deref(), Literal::Integer(_) | Literal::Float(_))
      }
      _ => false,
    }
  }

  /// Checks whether `self` is an integer or belongs to a complex type where integer is present too
  #[allow(dead_code)]
  pub(crate) fn is_supertype_of_integer(&self) -> bool {
    self.is_integer() || TypeImpl::new_unnamed(TypeKind::Integer).is_subtype_of(self)
  }

  /// Checks whether `self` is a float or belongs to a complex type where float is present too
  #[allow(dead_code)]
  pub(crate) fn is_supertype_of_float(&self) -> bool {
    self.is_float() || TypeImpl::new_unnamed(TypeKind::Float).is_subtype_of(self)
  }

  /// Checks whether `self` is an integer or a float, or belongs to a complex type where integer or
  /// float are present.
  #[allow(dead_code)]
  pub(crate) fn is_supertype_of_number(&self) -> bool {
    self.is_number() || TypeImpl::new_unnamed(TypeKind::Number).is_subtype_of(self)
  }

  /// Checks whether type is an integer number
  pub fn is_integer(&self) -> bool {
    match &self.kind {
      TypeKind::Integer | TypeKind::IntegerRange { .. } => true,
      TypeKind::Singleton { val } => {
        matches!(val.deref(), Literal::Integer(_))
      }
      _ => false,
    }
  }

  /// Checks whether type is a floating point number (or an integer, because compatible why not)
  pub fn is_float(&self) -> bool {
    match &self.kind {
      TypeKind::Float => true,
      TypeKind::Singleton { val } => {
        matches!(val.deref(), Literal::Float(_)) // | Literal::Integer(_) | Literal::BigInteger
      }
      _ => false,
    }
  }

  /// Checks whether type is a tuple type
  #[allow(dead_code)]
  pub(crate) fn is_tuple(&self) -> bool {
    matches!(
      &self.kind,
      TypeKind::AnyTuple | TypeKind::Tuple { .. } | TypeKind::IntegerRange { .. }
    )
  }

  /// Checks whether type is an union type
  pub fn is_union(&self) -> bool {
    matches!(&self.kind, TypeKind::Union { .. })
  }

  /// Checks whether type is a list
  pub fn is_list(&self) -> bool {
    match &self.kind {
      TypeKind::AnyList
      | TypeKind::List { .. }
      | TypeKind::StronglyTypedList { .. }
      | TypeKind::Nil => true,
      TypeKind::Singleton { val: singleton } => {
        matches!(singleton.deref(), Literal::List { .. } | Literal::String { .. })
      }
      _ => false,
    }
  }

  /// Checks whether type is an empty list (NIL)
  pub fn is_nil(&self) -> bool {
    matches!(&self.kind, TypeKind::Nil)
  }

  /// Checks whether type is a binary
  #[allow(dead_code)]
  pub(crate) fn is_binary(&self) -> bool {
    matches!(&self.kind, TypeKind::AnyBinary | TypeKind::Binary { .. })
  }

  /// Checks whether type is a map
  #[allow(dead_code)]
  pub(crate) fn is_map(&self) -> bool {
    matches!(&self.kind, TypeKind::AnyMap | TypeKind::Map { .. })
  }

  /// Checks whether a type is callable
  #[allow(dead_code)]
  pub(crate) fn is_function(&self) -> bool {
    matches!(
      &self.kind,
      TypeKind::AnyFn | TypeKind::Fn { .. } | TypeKind::FnRef { .. } | TypeKind::Lambda
    )
  }

  /// True if type is any()
  #[allow(dead_code)]
  pub(crate) fn is_any(&self) -> bool {
    matches!(&self.kind, TypeKind::Any)
  }

  /// True if type is none() or union of no types
  pub(crate) fn is_none(&self) -> bool {
    match &self.kind {
      TypeKind::None => true,
      TypeKind::Union(tu) if tu.is_empty() => true,
      _ => false,
    }
  }
}
