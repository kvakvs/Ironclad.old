use std::collections::{HashMap};

use crate::typing::erltype::{TypeVar, Type};

/// A map defines substitution of types by their names
#[derive(Debug, Clone)]
pub struct SubstitutionMap {
  pub types: HashMap<TypeVar, Type>,
}

impl SubstitutionMap {
  pub fn new() -> Self {
    Self { types: Default::default() }
  }
  pub fn from_two_lists(a: &Vec<TypeVar>, b: &Vec<Type>) -> Self {
    let types = a.iter()
        .zip(b.iter())
        .fold(HashMap::new(),
        |mut acc, (k, v)| {
          acc.insert(k.clone(), v.clone());
          acc
        });
    Self { types }
  }

  pub fn new_single(key: &TypeVar, value: &Type) -> Self {
    let mut types = HashMap::new();
    types.insert(key.clone(), value.clone());
    Self { types }
  }

  /// Composes this subst map with other map
  pub fn compose(&self, other: &SubstitutionMap) -> SubstitutionMap {
    let mut out = self.clone();
    other.types.iter().for_each(|(k, v)| {
      out.types.insert(k.clone(), v.clone());
    });
    return out
  }
}
