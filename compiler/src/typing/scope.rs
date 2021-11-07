//! Code to support variable scopes

use std::collections::HashMap;
use std::sync::{Arc, RwLock, Weak};
use crate::typing::erl_type::ErlType;

/// Contains identifiers known in the current scope
#[derive(Debug)]
pub struct Scope {
  /// Variables known to exist in the current scope
  pub variables: HashMap<String, Arc<ErlType>>,
  /// Reference to the parent scope for name search
  pub parent_scope: Weak<RwLock<Scope>>,
}

impl Default for Scope {
  fn default() -> Self {
    Self {
      variables: Default::default(),
      parent_scope: Default::default(),
    }
  }
}

impl Scope {
  /// Create a new empty scope
  pub fn empty(parent_scope: Weak<RwLock<Scope>>) -> Self {
    Self {
      variables: Default::default(),
      parent_scope,
    }
  }

  /// Create a new scope from a variable hashmap
  pub fn new(parent_scope: Weak<RwLock<Scope>>,
             variables: HashMap<String, Arc<ErlType>>) -> Self {
    Self {
      variables,
      parent_scope,
    }
  }

  /// Wrap self into `Arc<RwLock<>>`
  pub fn into_arc_rwlock(self) -> Arc<RwLock<Self>> {
    Arc::new(RwLock::new(self))
  }

  /// Return new copy of Scope with a new variable added
  pub fn add(&self, name: &str, t: Arc<ErlType>) -> Scope {
    let mut new_variables = self.variables.clone();
    new_variables.insert(String::from(name), t);
    Self::new(self.parent_scope.clone(), new_variables)
  }

  /// Retrieve variable type from scope
  pub fn get(&self, name: &str) -> Option<Arc<ErlType>> {
    self.variables.get(&String::from(name)).cloned()
  }
}
