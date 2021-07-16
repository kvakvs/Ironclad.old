use std::sync::atomic::{AtomicUsize, Ordering};
use lazy_static::lazy_static;
use crate::typing::erl_type::ErlType;

/// A type variable for not-yet-inferred types or generic types
/// Contains a name, and the type inferred so far (starts with Any)
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct TypeVar {
  /// Unique generated integer
  id: usize,
}

impl TypeVar {
  pub fn to_string(&self) -> String {
    format!("${}", self.id)
  }
}

lazy_static! {
    /// Counter to create unique TypeVar names
    static ref TYPEVAR_NUM: AtomicUsize = AtomicUsize::new(0);
}

impl TypeVar {
  pub fn new() -> Self {
    let new_id = TYPEVAR_NUM.fetch_add(1, Ordering::Acquire);
    Self {
      id: new_id}
  }

  pub fn get_type(&self) -> ErlType {
    unimplemented!("Read global typevar cache for type")
  }
}
