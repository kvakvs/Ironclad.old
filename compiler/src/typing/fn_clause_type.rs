//! Function clause type, component of function type
use std::sync::Arc;
use crate::typing::erl_type::ErlType;

/// Defines a function clause, with arguments and return type. Part of the function type.
#[derive(Debug)]
pub struct FnClauseType {
  /// Argument types for this function clause
  pub args: Vec<Arc<ErlType>>,
  /// Return type of this function clause
  pub ret_ty: Arc<ErlType>,
}

impl FnClauseType {
  /// Create a new function clause from just args
  pub fn new(args: Vec<Arc<ErlType>>, ret_ty: Arc<ErlType>) -> Self {
    Self { args, ret_ty }
  }
}
