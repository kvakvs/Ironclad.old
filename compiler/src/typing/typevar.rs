//! Defines a type variable, a unique numbered unnamed variable used in the Erlang code typing
use std::sync::atomic::{AtomicUsize, Ordering};
use lazy_static::lazy_static;
use std::fmt::Formatter;

/// A type variable for not-yet-inferred types or generic types
/// Contains a name, and the type inferred so far (starts with Any)
#[derive(Copy, Clone, PartialEq, Hash, Eq, Ord, PartialOrd)]
pub struct TypeVar(pub usize);

impl TypeVar {
  /// Create a new type variable with unique integer id (guarded by atomic usize)
  pub fn new() -> Self {
    let new_id = TYPEVAR_NUM.fetch_add(1, Ordering::Acquire);
    Self(new_id)
  }
}

lazy_static! {
    /// Counter to create unique TypeVar names
    static ref TYPEVAR_NUM: AtomicUsize = AtomicUsize::new(0);
    static ref SUBSCRIPT_NUMERICS: Vec<char> = vec!['₀','₁','₂','₃','₄','₅','₆','₇','₈','₉'];
}

impl std::fmt::Display for TypeVar {
  /// Format typevar as a nice string (sigma 𝞼 + number)
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "𝜎{}", self.0)
  }
}

impl std::fmt::Debug for TypeVar {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self) }
}

// impl Default for TypeVar {
//   /// Do not construct using Default
//   fn default() -> Self {
//     unreachable!("Do not call default on typevar, construct with ::new()")
//   }
// }
