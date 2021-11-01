//! Code to support variable scopes

use std::collections::HashMap;
use std::sync::Arc;
use crate::typing::erl_type::ErlType;

/// Contains identifiers known in the current scope
pub struct Scope {
  /// Variables known to exist in the current scope
  pub variables: HashMap<String, Arc<ErlType>>,
}

impl Scope {
  /// Create a new empty scope
  pub fn empty() -> Self {
    Self {
      variables: Default::default()
    }
  }

  /// Return new copy of Scope with a new variable added
  pub fn add(&self, name: &str, t: Arc<ErlType>) -> Scope {
    let mut new_variables = self.variables.clone();
    new_variables.insert(String::from(name), t);
    Scope {
      variables: new_variables
    }
  }
}
