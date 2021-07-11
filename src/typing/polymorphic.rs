use crate::typing::erltype::{TVar, Type};
use std::rc::Rc;
use std::collections::HashSet;

/// A polymorphic type based on multiple variables and a type which possibly uses these variables
pub struct Scheme {
  pub(crate) type_vars: HashSet<TVar>,
  pub(crate) ty: Rc<Type>,
}
