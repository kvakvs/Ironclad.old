//! Provides a `TypeEquation` struct for adding type equality/match constraints to the program.
//! `TypeEquation` defines a constraint: Type deduced must be a subtype or equal to the constraint.

use crate::typing::erl_type::ErlType;
use crate::source_loc::SourceLoc;
use std::fmt::Formatter;
use std::sync::atomic::{AtomicUsize, Ordering};
use lazy_static::lazy_static;
use std::sync::Arc;

lazy_static! {
    /// Counter to create unique TypeVar names
    static ref EQUATION_NUM: AtomicUsize = AtomicUsize::new(1);
}

/// Type equation, that `type_left` is equal or a subtype of `type_right`.
pub struct TypeEquation {
  /// Unique number of equation
  id: usize,
  /// Short explanation where this equation came from
  pub annotation: String,
  /// Left type of equation of t1 = t2, must match (be equal or subtype of) the right type
  /// This is the type which we deduced through the other means.
  pub type_left: Arc<ErlType>,
  /// Right type of equation of t1 = t2
  /// This is the type constraint. `type_left` must be equal or subtype of `type_right`.
  pub type_right: Arc<ErlType>,
  /// The reference to the source code which generated this equation
  pub location: SourceLoc,
}

impl std::fmt::Debug for TypeEquation {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{} from {}", self, self.annotation)
  }
}

impl std::fmt::Display for TypeEquation {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "Eq {}) {} ⊆ {}", self.id, self.type_left, self.type_right)
  }
}

impl TypeEquation {
  /// Create a new type equation
  pub fn new(location: SourceLoc, ty1: Arc<ErlType>, ty2: Arc<ErlType>, annotation: String) -> Self {
    let new_id = EQUATION_NUM.fetch_add(1, Ordering::Acquire);
    Self {
      id: new_id,
      type_left: ty1,
      type_right: ty2,
      location,
      annotation,
    }
  }
}
