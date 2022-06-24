//! Scope for module

use crate::erl_syntax::node::erl_var::ErlVar;
use crate::typing::erl_type::ErlType;
use libironclad_util::rw_hashmap::RwHashMap;
use std::collections::HashMap;
use std::sync::{Arc, Weak};

/// Contains identifiers known in the current scope.
/// For types and other module global stuff, see `RootScopeImpl` and `RootScope`
#[derive(Debug)]
pub struct ScopeImpl {
  /// A debug name used to distinguish between scopes while printing
  pub name: String,
  /// Variables known to exist in the current scope
  pub(crate) variables: RwHashMap<String, ErlType>,
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
    let vars_fmt = if let Ok(r_vars) = self.variables.collection.read() {
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
    ScopeImpl::empty(name, Weak::new())
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
    variables: HashMap<String, ErlType>,
  ) -> Scope {
    ScopeImpl {
      name,
      variables: RwHashMap::new(variables),
      parent_scope,
      // ..Default::default()
    }
    .into()
  }

  // /// Return new copy of Scope with a new variable added
  // #[allow(dead_code)]
  // pub(crate) fn add_clone(&self, var_name: &str) -> Scope {
  //   self.variables.add(var_name.to_string(), ErlTypeImpl::any());
  //   Self::new(self.name.clone(), self.parent_scope.clone(), new_variables)
  // }

  // /// Insert a new var into scope
  // #[allow(dead_code)]
  // pub(crate) fn add(&self, var_name: &str) {
  //   self.variables.add(var_name.to_string(), ErlTypeImpl::any())
  // }

  // /// Retrieve variable type from scope
  // #[allow(dead_code)]
  // pub(crate) fn get(&self, var_name: &str) -> Option<ErlType> {
  //   if let Ok(r_vars) = self.variables.read() {
  //     r_vars.get(&var_name.to_string()).cloned()
  //   } else {
  //     panic!("Can't lock scope to read variables")
  //   }
  // }

  /// Attempt to find a variable in the scope, or delegate to the parent scope
  #[allow(dead_code)]
  pub(crate) fn retrieve_var_from(&self, var: &ErlVar) -> Option<ErlType> {
    match self.variables.get(&var.name) {
      Some(var_type) => Some(var_type),
      None => match self.parent_scope.upgrade() {
        None => None,
        Some(parent) => Self::retrieve_var_from(&parent, var),
      },
    }
  }
}
