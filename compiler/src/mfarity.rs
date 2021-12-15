//! Defines a name/arity pair to refer to local functions in a module

use std::fmt::Formatter;

/// Points to a function in the current module
#[derive(Clone, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub struct MFArity {
  /// Belongs to `Some(module_name)`, or `None` if local function
  pub module: Option<String>,
  /// Function name atom, as string
  pub name: String,
  /// Function argument count
  pub arity: usize,
}

impl MFArity {
  /// Creates a new local (no module) funarity
  pub fn new_local_from_string(name: String, arity: usize) -> Self {
    MFArity {
      module: None,
      name,
      arity,
    }
  }

  /// Creates a new local (no module) funarity, cloning the name
  pub fn new_local(name: &str, arity: usize) -> Self {
    MFArity {
      module: None,
      name: String::from(name),
      arity,
    }
  }
}

impl std::fmt::Debug for MFArity {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self) }
}

impl std::fmt::Display for MFArity {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match &self.module {
      None => write!(f, "{}/{}", self.name, self.arity),
      Some(m) => write!(f, "{}:{}/{}", m, self.name, self.arity),
    }
  }
}