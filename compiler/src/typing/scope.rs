//! Code to support variable scopes

use std::collections::HashMap;
use std::fmt::Formatter;
use std::sync::{Arc, RwLock, Weak};
use crate::core_erlang::syntax_tree::node::core_var::Var;
use crate::mfarity::MFArity;
use crate::typing::erl_type::ErlType;

/// Contains identifiers known in the current scope
#[derive(Debug)]
pub struct Scope {
  /// A debug name used to distinguish between scopes while printing
  pub name: String,

  /// Variables known to exist in the current scope
  pub variables: HashMap<String, Arc<ErlType>>,

  /// Functions can only be found on the module root scope (but technically can be created in the
  /// other internal scopes too)
  pub functions: HashMap<MFArity, Arc<ErlType>>,

  /// Types defined in the global module scope. Using typename/arity as key in type hierarchy
  pub typedefs: HashMap<MFArity, Arc<ErlType>>,

  /// Reference to the parent scope for name search
  pub parent_scope: Weak<RwLock<Scope>>,

  // TODO: Thrown types and catched types can be part of scope
}

impl Default for Scope {
  fn default() -> Self {
    Self {
      name: "default_scope".to_string(),
      variables: Default::default(),
      functions: Default::default(),
      typedefs: Default::default(),
      parent_scope: Default::default(),
    }
  }
}

impl std::fmt::Display for Scope {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    let vars_fmt = self.variables.iter()
        .map(|v| format!("{}={}", v.0, v.1))
        .collect::<Vec<String>>()
        .join(", ");
    let funs_fmt = self.functions.iter()
        .map(|fnc| format!("{}", fnc.0))
        .collect::<Vec<String>>()
        .join(", ");
    write!(f, "Scope{{ \"{}\", vars [{}], funs [{}] }}", self.name, vars_fmt, funs_fmt)
  }
}

impl Scope {
  /// Default empty scope for the module root
  pub fn new_root_scope(name: String) -> Arc<RwLock<Self>> {
    Scope::empty(name, Weak::new()).into_arc_rwlock()
  }

  /// Create a new empty scope
  pub fn empty(name: String, parent_scope: Weak<RwLock<Scope>>) -> Self {
    Self {
      name,
      variables: Default::default(),
      functions: Default::default(),
      typedefs: Default::default(),
      parent_scope,
    }
  }

  /// Create a new scope from a variable hashmap
  pub fn new(name: String,
             parent_scope: Weak<RwLock<Scope>>,
             variables: HashMap<String, Arc<ErlType>>) -> Self {
    Self {
      name,
      variables,
      functions: Default::default(),
      typedefs: Default::default(),
      parent_scope,
    }
  }

  /// Wrap self into `Arc<RwLock<>>`
  pub fn into_arc_rwlock(self) -> Arc<RwLock<Self>> {
    Arc::new(RwLock::new(self))
  }

  /// Return new copy of Scope with a new variable added
  pub fn add(&self, var_name: &str) -> Scope {
    let mut new_variables = self.variables.clone();
    new_variables.insert(var_name.to_string(), ErlType::any());
    Self::new(self.name.clone(), self.parent_scope.clone(), new_variables)
  }

  /// Insert a new var into scope
  pub fn add_to(scope: &RwLock<Scope>, var_name: &str) {
    if let Ok(mut scope_w) = scope.write() {
      scope_w.variables.insert(var_name.to_string(), ErlType::any());
    }
  }

  /// Retrieve variable type from scope
  pub fn get(&self, var_name: &str) -> Option<Arc<ErlType>> {
    self.variables.get(&var_name.to_string())
        .map(|var_type| var_type.clone())
  }

  /// Attempt to find a variable in the scope, or delegate to the parent scope
  pub fn retrieve_var_from(scope: &RwLock<Scope>, var: &Var) -> Option<Arc<ErlType>> {
    if let Ok(scope_read) = scope.read() {
      match scope_read.variables.get(&var.name) {
        Some(var_type) => Some(var_type.clone()),
        None => match scope_read.parent_scope.upgrade() {
          None => None,
          Some(parent) => Self::retrieve_var_from(&parent, var)
        }
      }
    } else {
      None
    }
  }

  /// Attempt to find a function in the scope, or delegate to the parent scope
  pub fn retrieve_fn_from(scope: &RwLock<Scope>, mfa: &MFArity) -> Option<Arc<ErlType>> {
    if let Ok(scope_read) = scope.read() {
      match scope_read.functions.get(mfa) {
        Some(val) => Some(val.clone()),
        None => match scope_read.parent_scope.upgrade() {
          None => None,
          Some(parent) => Self::retrieve_fn_from(&parent, mfa)
        }
      }
    } else {
      None
    }
  }

  /// Add a function by MFA and its type
  pub fn add_function(scope: &RwLock<Scope>, mfa: &MFArity, ty: Arc<ErlType>) {
    if let Ok(mut scope_w) = scope.write() {
      scope_w.functions.insert(mfa.clone(), ty);
    }
  }

  // /// Report usages of an expression in the current scope. This propagates up from deepest scopes.
  // /// The result will be the scope updated to a possibly more narrow type.
  // /// For example if initial variable was given type `any()` but after seeing it used in a `a+b`
  // /// operation as `integer()` and maybe in another operation as `float()`, the type might be
  // /// updated to a `number()`.
  // pub fn record_value_usages(scope: &RwLock<Scope>, ast: &CoreAst, usages: &[Arc<ErlType>]) {
  //   if let CoreAst::Var(v) = ast {
  //     if let Ok(mut scope_w) = scope.write() {
  //       scope_w.record_value_usages_1(v, usages);
  //     }
  //   }
  // }

  // /// Try narrow the variable type in the scope
  // fn record_value_usages_1(&mut self, v: &Var, usages: &[Arc<ErlType>]) -> ErlResult<()> {
  //   assert!(self.variables.contains_key(&v.name), "Scope must contain variable {}", v.name);
  //
  //   if let Some(var_type) = self.variables.get_mut(&v.name) {
  //     if let Some(updated_type) = var_type.clone().narrow_type(usages)? {
  //       self.variables.insert(v.name.clone(), updated_type);
  //     }
  //   }
  //   Ok(())
  // }
}
