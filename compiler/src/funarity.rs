//! Defines a name/arity pair to refer to local functions in a module

#[derive(Hash, PartialEq)]
pub struct FunArity {
  pub name: String,
  pub arity: usize,
}
