use crate::typing::erltype::{TypeVar, Type};

/// A polymorphic type based on multiple variables and a type which possibly uses these variables
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Scheme {
  pub type_vars: Vec<TypeVar>,
  pub ty: Type,
}

impl Scheme {
  pub(crate) fn new_single_empty(ty: Type) -> Self {
    Self {
      ty,
      type_vars: vec![],
    }
  }
}
