use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::typing::erltype::{TVar, Type};

/// A map defines substitution of types by their names
#[derive(Debug)]
pub struct Subst {
  pub types: HashMap<TVar, Rc<Type>>,
}

impl Subst {
  pub fn new() -> Self {
    Self { types: Default::default() }
  }

  pub fn compose(&mut self, other: &Subst) {
    other.types.iter().for_each(|(k, v)| {
      self.types[k] = v.clone()
    })
  }
}
