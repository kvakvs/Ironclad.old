use std::collections::hash_map::Entry;
use std::collections::HashMap;

use crate::typing::erltype::TypeVar;
use crate::typing::polymorphic::Scheme;

/// A type scope, all types known in a scope
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeEnv {
  pub(crate) env: HashMap<TypeVar, Scheme>
}

impl TypeEnv {
  /// Create a new empty typeenv
  pub fn new() -> Self {
    Self{ env: Default::default() }
  }

  /// Retrieve a polymorphic type scheme by name
  pub fn get<'a>(&mut self, tvar: TypeVar) -> Option<Scheme> {
    match self.env.entry(tvar) {
      Entry::Occupied(e) => Some(e.get().clone()),
      Entry::Vacant(_) => None
    }
  }

  pub fn extend(&self, key: TypeVar, scheme: Scheme) -> Self {
    let mut env = self.env.clone();
    env.insert(key, scheme);
    Self { env }
  }
}
