//! Collection of MFArity which is lockable

use crate::pretty::Pretty;
use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::sync::RwLock;

/// Set of generic value type, protected by a `RwLock`
#[derive(Debug)]
pub struct RwHashSet<ValType: Hash> {
  /// The lockable array
  pub collection: RwLock<HashSet<ValType>>,
}

impl<ValType: Hash + Display> Display for RwHashSet<ValType> {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    if let Ok(r_collection) = self.collection.read() {
      Pretty::display_square_list(r_collection.iter(), f)
    } else {
      panic!("Can't lock ModuleAttributes for printing")
    }
  }
}

impl<ValType: Hash> Default for RwHashSet<ValType> {
  fn default() -> Self {
    Self { collection: RwLock::new(HashSet::default()) }
  }
}

impl<ValType: Hash + Eq> RwHashSet<ValType> {
  /// Contained data length
  pub fn len(&self) -> usize {
    if let Ok(r_collection) = self.collection.read() {
      r_collection.len()
    } else {
      panic!("Can't lock RwHashSet for length check")
    }
  }

  /// Check whether an item exists
  pub fn contains(&self, key: &ValType) -> bool {
    if let Ok(r_collection) = self.collection.read() {
      r_collection.contains(key)
    } else {
      panic!("Can't lock RwHashSet for reading")
    }
  }

  /// Inserts an item into a set
  pub fn add(&self, item: ValType) {
    if let Ok(mut w_collection) = self.collection.write() {
      w_collection.insert(item);
    } else {
      panic!("Can't lock RwHashSet to insert a new one")
    }
  }
}
