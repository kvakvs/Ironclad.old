//! MFArity-like name/arity pair

/// A key to preprocessor defines dictionary, as defines can coexist with same name but different
/// number of args
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct NameArity {
  /// Name for namearity pair
  pub name: String,
  /// The count of arguments
  pub arity: usize,
}

impl NameArity {
  /// Create a new Name-arity pair
  pub(crate) fn new(name: &str, arity: usize) -> NameArity {
    NameArity { name: name.to_string(), arity }
  }
}
