//! Defines a name/arity pair to refer to local functions in a module

/// Points to a function in the current module
#[derive(Hash, PartialEq, Eq)]
pub struct FunArity {
  /// Function name atom, as string
  pub name: String,
  /// Function argument count
  pub arity: usize,
}
