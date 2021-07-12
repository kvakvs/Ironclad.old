use crate::typing::erltype::{TVar, Type};

/// A polymorphic type based on multiple variables and a type which possibly uses these variables
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Scheme {
  pub type_vars: Vec<TVar>,
  pub ty: Type,
}
