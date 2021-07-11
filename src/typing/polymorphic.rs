use crate::typing::erltype::{TVar, Type};
use std::collections::HashSet;

/// A polymorphic type based on multiple variables and a type which possibly uses these variables
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Scheme {
  pub(crate) type_vars: HashSet<TVar>,
  pub(crate) ty: Type,
}
