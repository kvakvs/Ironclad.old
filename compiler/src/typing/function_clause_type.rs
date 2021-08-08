use crate::typing::erl_type::ErlType;
use std::fmt::Formatter;
use crate::display::display_comma_separated;

/// Describes one function clause arg and return types.
pub struct FunctionClauseType {
  /// Argument types for this clause
  pub arg_types: Vec<ErlType>,
  /// Return type for this clause
  pub ret_ty: Box<ErlType>,
}

impl FunctionClauseType {
  pub fn new(arg_types: Vec<ErlType>, ret_ty: ErlType) -> Self {
    Self {
      arg_types,
      ret_ty: Box::new(ret_ty),
    }
  }
}

impl std::fmt::Display for FunctionClauseType {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "(")?;
    display_comma_separated(&self.arg_types, f)?;
    write!(f, " -> {}", self.ret_ty)
  }
}