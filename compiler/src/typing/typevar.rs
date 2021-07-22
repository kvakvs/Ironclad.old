//! Defines a type variable, a unique numbered unnamed variable used in the Erlang code typing
use std::sync::atomic::{AtomicUsize, Ordering};
use lazy_static::lazy_static;

/// A type variable for not-yet-inferred types or generic types
/// Contains a name, and the type inferred so far (starts with Any)
#[derive(Clone, PartialEq, Hash, Eq)]
pub struct TypeVar(usize);

lazy_static! {
    /// Counter to create unique TypeVar names
    static ref TYPEVAR_NUM: AtomicUsize = AtomicUsize::new(0);
    static ref SUBSCRIPT_NUMERICS: Vec<char> = vec!['₀','₁','₂','₃','₄','₅','₆','₇','₈','₉'];
}

impl TypeVar {
  // fn subscript(n: usize) -> String {
  //   format!("{}", n).drain(..)
  //       .map(|c| SUBSCRIPT_NUMERICS[c as usize - 48]) // guarantee the input is 0..9
  //       .collect()
  // }

  /// Format typevar as a nice string (sigma 𝞼 + number)
  pub fn to_string(&self) -> String {
    // displayed as T₂ or 𝜎₂
    // format!("T{}", Self::subscript(self.0))
    format!("𝜎{}", self.0)
  }
}

impl TypeVar {
  /// Create a new type variable with unique integer id (guarded by atomic usize)
  pub fn new() -> Self {
    let new_id = TYPEVAR_NUM.fetch_add(1, Ordering::Acquire);
    Self(new_id)
  }
}
