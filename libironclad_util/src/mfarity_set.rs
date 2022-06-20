//! Collection of MFArity which is lockable

use crate::mfarity::MFArity;
use std::collections::HashSet;
use std::sync::RwLock;

/// Collection of fun-arities
#[derive(Debug)]
pub struct MFAritySet {
  /// The lockable array
  pub collection: RwLock<HashSet<MFArity>>,
}

impl Default for MFAritySet {
  fn default() -> Self {
    Self { collection: RwLock::new(HashSet::default()) }
  }
}

impl MFAritySet {
  /// Contained data length
  pub fn len(&self) -> usize {
    if let Ok(r_collection) = self.collection.read() {
      r_collection.len()
    } else {
      panic!("Can't lock array of mfarities for length check")
    }
  }

  pub fn contains(&self, key: &MFArity) -> bool {
    if let Ok(r_collection) = self.collection.read() {
      r_collection.contains(key)
    } else {
      panic!("Can't lock array of mfarities for reading")
    }
  }
}
