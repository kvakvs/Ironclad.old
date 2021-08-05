//! Provides a TypeEquation struct for adding type equality/match constraints to the program

use crate::typing::erl_type::ErlType;
use crate::source_loc::SourceLoc;
use std::fmt::Formatter;

/// Type equation, assumes matching or equal types, t1 = t2
pub struct TypeEquation {
  /// Short explanation where this equation came from
  pub annotation: String,
  /// Left type of equation of t1 = t2, must equal (match) the right type
  pub left: ErlType,
  /// Right type of equation of t1 = t2
  pub right: ErlType,
  /// The reference to the source code which generated this equation
  pub location: SourceLoc,
}

impl std::fmt::Debug for TypeEquation {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self) }
}

impl std::fmt::Display for TypeEquation {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{} ⊆ {} ({})", self.left, self.right, self.annotation)
  }
}

impl TypeEquation {
  /// Create a new type equation
  pub fn new(location: SourceLoc, ty1: ErlType, ty2: ErlType, annotation: &str) -> Self {
    Self {
      left: ty1,
      right: ty2,
      location,
      annotation: String::from(annotation),
    }
  }
}
