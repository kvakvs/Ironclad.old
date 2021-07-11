use std::borrow::Borrow;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::rc::Rc;

use crate::typing::erltype::TVar;
use crate::typing::polymorphic::Scheme;
use crate::typing::subst::Subst;

/// A type scope, all types known in a scope
pub struct TypeEnv {
  env: HashMap<TVar, Rc<Scheme>>
}

impl TypeEnv {
  /// Create a new empty typeenv
  pub fn new() -> Self {
    Self{ env: Default::default() }
  }

  /// Retrieve a polymorphic type scheme by name
  pub fn get<'a>(&mut self, tvar: TVar) -> Option<Rc<Scheme>> {
    match self.env.entry(tvar) {
      Entry::Occupied(e) => Some(e.get().clone()),
      Entry::Vacant(_) => None
    }
  }
}
