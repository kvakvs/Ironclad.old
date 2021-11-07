//! A variable with possibly missing name and unique typevar
use std::fmt::Formatter;
use std::sync::Arc;
use lazy_static::lazy_static;
use std::sync::atomic::{AtomicUsize, Ordering};
use crate::source_loc::SourceLoc;
use crate::typing::erl_type::ErlType;

lazy_static! {
    /// Counter to create unique unique Var names
    static ref UNIQ_VAR_NUM: AtomicUsize = AtomicUsize::new(0);
}

/// Represents a variable with an optional name (`None` for generated variables), or a string name,
/// and a new unique type variable.
#[derive(Debug)]
pub struct Var {
  /// Source file pointer
  pub location: SourceLoc,
  /// Variable name, numbered unnamed variables are pre-formatted to strings, for simplicity
  pub name: String,
}

impl Var {
  /// Creates a new prefixed variable with unique numbering
  pub fn new_unique(location: SourceLoc, prefix: &str) -> Self {
    let new_id = UNIQ_VAR_NUM.fetch_add(1, Ordering::Acquire);
    Self {
      location,
      name: format!("@{}{}", prefix, new_id),
    }
  }

  /// A default guessed type for var is `any()` we will reiterate and make it more a narrow type at
  /// a later stage, as we learn more usage details.
  pub fn synthesize_type() -> Arc<ErlType> {
    ErlType::Any.into()
  }
}

impl std::fmt::Display for Var {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.name)
  }
}