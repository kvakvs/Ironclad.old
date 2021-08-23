//! Declare a struct describing a type of one function clause. A function type can have many clause
//! types. Return type of a function is union of all clause return types.
use std::sync::Arc;
use std::fmt::Formatter;
use crate::typing::erl_type::ErlType;
use crate::display::display_comma_separated;

/// Describes one function clause arg and return types.
#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct FnClauseType {
  /// Argument types for this clause
  pub arg_types: Vec<Arc<ErlType>>,
  /// Return type for this clause
  pub ret_ty: Arc<ErlType>,
}

impl FnClauseType {
  /// Create a new function clause type
  pub fn new(arg_types: Vec<Arc<ErlType>>, ret_ty: Arc<ErlType>) -> Self {
    Self { arg_types, ret_ty }
  }
}

impl std::fmt::Display for FnClauseType {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "clause: (")?;
    display_comma_separated(&self.arg_types, f)?;
    write!(f, ") -> {}", self.ret_ty)
  }
}