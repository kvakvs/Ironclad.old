//! Provides a TypeEquation struct for adding type equality/match constraints to the program

use crate::typing::erl_type::ErlType;
use crate::source_loc::SourceLoc;

/// Type equation, assumes matching or equal types, t1 = t2
pub struct TypeEquation {
  /// Left type of equation of t1 = t2, must equal (match) the right type
  pub left: ErlType,
  /// Right type of equation of t1 = t2
  pub right: ErlType,
  /// The reference to the source code which generated this equation
  pub location: SourceLoc,
}

impl TypeEquation {
  /// Create a new type equation
  pub fn new(location: SourceLoc, ty1: ErlType, ty2: ErlType) -> Self {
    Self {
      left: ty1,
      right: ty2,
      location,
    }
  }
}
