//! A variable with possibly missing name and unique typevar
use lazy_static::lazy_static;
use crate::typing::typevar::TypeVar;
use std::sync::atomic::{AtomicUsize, Ordering};
use crate::source_loc::SourceLoc;

lazy_static! {
    /// Counter to create unique unique Var names
    static ref UNIQ_VAR_NUM: AtomicUsize = AtomicUsize::new(0);
}

/// Represents a variable with an optional name (`None` for generated variables), or a string name,
/// and a new unique type variable.
pub struct Var {
  /// Source file pointer
  pub location: SourceLoc,
  /// Optional name, `None` means the name is numbered from `Self::ty`
  pub name: Option<String>,
  /// Unique type variable
  pub ty: TypeVar,
}

impl Var {
  /// Creates a new prefixed variable with unique numbering
  pub fn new_unique(location: SourceLoc, prefix: &str) -> Self {
    let new_id = UNIQ_VAR_NUM.fetch_add(1, Ordering::Acquire);
    Self {
      location,
      name: Some(format!("@{}{}", prefix, new_id)),
      ty: TypeVar::new(),
    }
  }
}
