//! Code to support variable scopes

use std::collections::HashMap;
use std::sync::{Arc, RwLock, Weak};
use crate::core_erlang::syntax_tree::node::var::Var;
use crate::typing::erl_type::ErlType;

/// Contains identifiers known in the current scope
#[derive(Debug)]
pub struct Scope {
  /// Variables known to exist in the current scope
  pub variables: HashMap<String, Arc<Var>>,
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
  /// Default empty scope for the module root
  pub fn new_root_scope() -> Arc<RwLock<Self>> {
    Scope::empty(Weak::new()).into_arc_rwlock()
  }

  /// Create a new empty scope
  pub fn empty(parent_scope: Weak<RwLock<Scope>>) -> Self {
    Self {
      variables: Default::default(),
      parent_scope,
    }
  }

  /// Create a new scope from a variable hashmap
  pub fn new(parent_scope: Weak<RwLock<Scope>>,
             variables: HashMap<String, Arc<Var>>) -> Self {
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
  pub fn add(&self, var: &Arc<Var>) -> Scope {
    let mut new_variables = self.variables.clone();
    new_variables.insert(var.name.clone(), var.clone());
    Self::new(self.parent_scope.clone(), new_variables)
  }

  /// Retrieve variable type from scope
  pub fn get(&self, var: &Arc<Var>) -> Option<Arc<ErlType>> {
    self.variables.get(&var.name)
        .map(|scope_var| scope_var.ty.clone())
  }
}
