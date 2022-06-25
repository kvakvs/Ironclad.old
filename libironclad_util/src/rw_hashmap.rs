//! Collection of MFArity which is lockable

use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::sync::RwLock;

/// Collection of fun-arities, protected by a `RwLock`
#[derive(Debug)]
pub struct RwHashMap<KeyType, ValType> {
  /// The lockable hashmap
  pub collection: RwLock<HashMap<KeyType, ValType>>,
}

impl<KeyType: Display, ValType: Display> Display for RwHashMap<KeyType, ValType> {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    if let Ok(r_collection) = self.collection.read() {
      for (k, v) in r_collection.iter() {
        writeln!(f, "{} = {}; ", k, v)?;
      }
      Ok(())
    } else {
      panic!("Can't lock RwHashMap for printing")
    }
  }
}

impl<KeyType, ValType> Default for RwHashMap<KeyType, ValType> {
  fn default() -> Self {
    Self { collection: RwLock::new(HashMap::default()) }
  }
}

impl<KeyType: Clone + Eq + Hash, ValType: Clone> RwHashMap<KeyType, ValType> {
  pub fn new(collection: HashMap<KeyType, ValType>) -> Self {
    Self { collection: RwLock::new(collection) }
  }

  /// Contained data length
  pub fn len(&self) -> usize {
    if let Ok(r_collection) = self.collection.read() {
      r_collection.len()
    } else {
      panic!("Can't lock RwHashMap for length check")
    }
  }

  /// Check whether an item exists
  pub fn contains(&self, key: &KeyType) -> bool {
    if let Ok(w_collection) = self.collection.write() {
      w_collection.contains_key(key)
    } else {
      panic!("Can't lock RwHashMap for reading")
    }
  }

  /// Inserts an item into a set
  pub fn add(&self, key: KeyType, item: ValType) {
    if let Ok(mut w_collection) = self.collection.write() {
      w_collection.insert(key, item);
    } else {
      panic!("Can't lock RwHashMap to insert a new one")
    }
  }

  /// Deletes an item, if a predicate returns true
  pub fn delete_if(&self, predicate: impl Fn(&KeyType, &ValType) -> bool) {
    if let Ok(mut w_collection) = self.collection.write() {
      let mut remove_candidates = Vec::<KeyType>::new();

      for (key, val) in w_collection.iter() {
        if predicate(key, val) {
          remove_candidates.push(key.clone());
        }
      }

      for k in remove_candidates.iter() {
        let _ = w_collection.remove(k);
      }
    } else {
      panic!("Can't lock RwHashMap to insert a new one")
    }
  }

  /// Retrieve an item
  pub fn get(&self, key: &KeyType) -> Option<ValType> {
    if let Ok(r_collection) = self.collection.read() {
      r_collection.get(key).cloned()
    } else {
      panic!("Can't lock RwHashMap to insert a new one")
    }
  }
}
