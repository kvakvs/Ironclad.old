//! Function clause type, component of function type
use std::sync::Arc;
use crate::typing::erl_type::ErlType;

/// Defines a function clause, with arguments and return type.
/// Use 1 or multiple `FnClauseType` to construct a function type.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FnClauseType {
  /// Argument types for this function clause
  args: Vec<Arc<ErlType>>,
  /// Return type of this function clause
  ret_ty: Arc<ErlType>,
}

impl FnClauseType {
  /// Retrieve return type
  pub fn ret_ty(&self) -> &Arc<ErlType> { &self.ret_ty }

  /// Create a new function clause from just args
  pub fn new(args: Vec<Arc<ErlType>>, ret_ty: Arc<ErlType>) -> Self {
    Self { args, ret_ty }
  }

  /// Get the function clause argument count
  pub fn arity(&self) -> usize { self.args.len() }

  /// Check whether calling any clause of `supertype` function type would be compatible with calling
  /// this clause with the same args.
  pub fn is_any_clause_compatible(&self, supertype: &[Arc<FnClauseType>]) -> bool {
    supertype.iter()
        .any(|sup| self.is_clause_compatible(sup))
  }

  /// Check whether `self` is a subtype of super_clause (i.e. if calling `self`, will be compatible
  /// with calling `super_clause` with the same args.
  fn is_clause_compatible(&self, super_clause: &FnClauseType) -> bool {
    self.args.iter()
        .zip(super_clause.args.iter())
        .all(|(sub_arg, super_arg)| sub_arg.is_subtype_of(super_arg))
    && self.ret_ty.is_subtype_of(&super_clause.ret_ty)
  }

  /// Check whether argument list can be passed to this clause
  pub fn can_accept_args(&self, args: &[Arc<ErlType>]) -> bool {
    self.args.iter().zip(args.iter())
        .all(|(in_arg, my_arg)| in_arg.is_subtype_of(my_arg))
  }
}
