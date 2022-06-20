//! Scope for module

use crate::erl_syntax::node::erl_var::ErlVar;
use crate::typing::erl_type::ErlType;
use std::collections::HashMap;
use std::sync::{Arc, RwLock, Weak};

/// Contains identifiers known in the current scope.
/// For types and other module global stuff, see `RootScopeImpl` and `RootScope`
#[derive(Debug)]
pub struct ScopeImpl {
  /// A debug name used to distinguish between scopes while printing
  pub name: String,
  /// Variables known to exist in the current scope
  pub(crate) variables: RwLock<HashMap<String, Arc<ErlType>>>,
  /// Reference to the parent scope for name search
  pub parent_scope: Weak<ScopeImpl>,
}

/// Alias type for `Arc<>`
pub type Scope = Arc<ScopeImpl>;

impl Default for ScopeImpl {
  fn default() -> Self {
    Self {
      name: "default_scope".to_string(),
      variables: Default::default(),
      parent_scope: Default::default(),
    }
  }
}

impl std::fmt::Display for ScopeImpl {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let vars_fmt = if let Ok(r_vars) = self.variables.read() {
      r_vars
        .iter()
        .map(|v| format!("{}={}", v.0, v.1))
        .collect::<Vec<String>>()
        .join(", ")
    } else {
      panic!("Can't lock Scope to print vars")
    };
    write!(f, "Scope{{ \"{}\", vars [{}], }}", self.name, vars_fmt,)
  }
}

impl ScopeImpl {
  /// Default empty scope for the module root
  pub fn new_root_scope(name: String) -> Scope {
    ScopeImpl::empty(name, Weak::new()).into()
  }

  /// Create a new empty scope
  #[allow(dead_code)]
  pub(crate) fn empty(name: String, parent_scope: Weak<ScopeImpl>) -> Scope {
    ScopeImpl { name, parent_scope, ..Default::default() }.into()
  }

  /// Create a new scope from a variable hashmap
  pub(crate) fn new(
    name: String,
    parent_scope: Weak<ScopeImpl>,
    variables: HashMap<String, Arc<ErlType>>,
  ) -> Scope {
    ScopeImpl {
      name,
      variables: RwLock::new(variables),
      parent_scope,
      ..Default::default()
    }
    .into()
  }

  /// Return new copy of Scope with a new variable added
  #[allow(dead_code)]
  pub(crate) fn add(&self, var_name: &str) -> Scope {
    if let Ok(r_vars) = self.variables.read() {
      let mut new_variables = r_vars.clone();
      new_variables.insert(var_name.to_string(), ErlType::any());
      Self::new(self.name.clone(), self.parent_scope.clone(), new_variables)
    } else {
      panic!("Can't lock Scope to clone and update variables")
    }
  }

  /// Insert a new var into scope
  #[allow(dead_code)]
  pub(crate) fn add_to(&self, var_name: &str) {
    if let Ok(mut w_vars) = self.variables.write() {
      w_vars.insert(var_name.to_string(), ErlType::any());
    } else {
      panic!("Can't lock Scope to update vars")
    }
  }

  /// Retrieve variable type from scope
  #[allow(dead_code)]
  pub(crate) fn get(&self, var_name: &str) -> Option<Arc<ErlType>> {
    if let Ok(r_vars) = self.variables.read() {
      r_vars.get(&var_name.to_string()).cloned()
    } else {
      panic!("Can't lock scope to read variables")
    }
  }

  /// Attempt to find a variable in the scope, or delegate to the parent scope
  pub(crate) fn retrieve_var_from(&self, var: &ErlVar) -> Option<Arc<ErlType>> {
    if let Ok(r_vars) = self.variables.read() {
      match r_vars.get(&var.name) {
        Some(var_type) => Some(var_type.clone()),
        None => match self.parent_scope.upgrade() {
          None => None,
          Some(parent) => Self::retrieve_var_from(&parent, var),
        },
      }
    } else {
      panic!("Can't lock scope to access variables")
    }
  }
}
