//! Collection of MFArity which is lockable

use crate::mfarity::MFArity;
use std::collections::HashSet;
use std::sync::RwLock;

/// Collection of fun-arities, protected by a `RwLock`
#[derive(Debug)]
pub struct RwMFAritySet {
  /// The lockable array
  pub collection: RwLock<HashSet<MFArity>>,
}

impl Default for RwMFAritySet {
  fn default() -> Self {
    Self { collection: RwLock::new(HashSet::default()) }
  }
}

impl RwMFAritySet {
  /// Contained data length
  pub fn len(&self) -> usize {
    if let Ok(r_collection) = self.collection.read() {
      r_collection.len()
    } else {
      panic!("Can't lock array of mfarities for length check")
    }
  }

  /// Check whether an item exists
  pub fn contains(&self, key: &MFArity) -> bool {
    if let Ok(r_collection) = self.collection.read() {
      r_collection.contains(key)
    } else {
      panic!("Can't lock array of mfarities for reading")
    }
  }

  /// Inserts an item into a set
  pub fn add(&self, item: MFArity) {
    if let Ok(mut w_collection) = self.collection.write() {
      w_collection.insert(item);
    } else {
      panic!("Can't lock array of mfarities to insert a new one")
    }
  }
}
