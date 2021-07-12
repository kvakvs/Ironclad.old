use std::collections::{HashMap};

use crate::typing::erltype::{TVar, Type};

/// A map defines substitution of types by their names
#[derive(Debug)]
pub struct SubstitutionMap {
  pub types: HashMap<TVar, Type>,
}

impl SubstitutionMap {
  pub fn new() -> Self {
    Self { types: Default::default() }
  }
  pub fn from_two_lists(a: &Vec<TVar>, b: &Vec<Type>) -> Self {
    let types = a.iter()
        .zip(b.iter())
        .fold(HashMap::new(),
        |mut acc, (k, v)| {
          acc.insert(k.clone(), v.clone());
          acc
        });
    Self { types }
  }

  pub fn new_single(key: &TVar, value: &Type) -> Self {
    let mut types = HashMap::new();
    types.insert(key.clone(), value.clone());
    Self { types }
  }

  /// Composes this subst map with other map in place. Result is stored in this map.
  pub fn compose(&mut self, other: &SubstitutionMap) {
    other.types.iter().for_each(|(k, v)| {
      self.types.insert(k.clone(), v.clone());
    })
  }
}
