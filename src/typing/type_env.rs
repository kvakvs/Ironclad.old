use std::collections::hash_map::Entry;
use std::collections::HashMap;

use crate::typing::erltype::TVar;
use crate::typing::polymorphic::Scheme;

/// A type scope, all types known in a scope
pub struct TypeEnv {
  pub(crate) env: HashMap<TVar, Scheme>
}

impl TypeEnv {
  /// Create a new empty typeenv
  pub fn new() -> Self {
    Self{ env: Default::default() }
  }

  /// Retrieve a polymorphic type scheme by name
  pub fn get<'a>(&mut self, tvar: TVar) -> Option<Scheme> {
    match self.env.entry(tvar) {
      Entry::Occupied(e) => Some(e.get().clone()),
      Entry::Vacant(_) => None
    }
  }
}
