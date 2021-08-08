//! Defines a name/arity pair to refer to local functions in a module

use std::fmt::Formatter;

/// Points to a function in the current module
#[derive(Clone, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub struct FunArity {
  /// Function name atom, as string
  pub name: String,
  /// Function argument count
  pub arity: usize,
}

impl FunArity {
  /// Creates a new funarity
  pub fn new(name: String, arity: usize) -> Self {
    FunArity { name, arity }
  }

  /// Creates a new funarity, cloning the name
  pub fn new_str(name: &str, arity: usize) -> Self {
    FunArity { name: String::from(name), arity }
  }
}

impl std::fmt::Debug for FunArity {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self) }
}

impl std::fmt::Display for FunArity {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}/{}", self.name, self.arity)
  }
}