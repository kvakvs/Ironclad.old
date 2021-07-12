use std::collections::{HashMap};
use std::rc::Rc;

use crate::typing::erltype::{TVar, Type};

/// A map defines substitution of types by their names
#[derive(Debug)]
pub struct SubstitutionMap {
  pub types: HashMap<TVar, Rc<Type>>,
}

impl SubstitutionMap {
  pub fn new() -> Self {
    Self { types: Default::default() }
  }

  pub fn compose(&mut self, other: &SubstitutionMap) {
    other.types.iter().for_each(|(k, v)| {
      self.types.insert(k.clone(), v.clone());
    })
  }
}
