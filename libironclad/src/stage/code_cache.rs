//! Code cache contains compile states in a dict
use crate::project::module::ErlModule;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

/// Contains cache of compile state keyed by filename
pub struct ErlCodeCache {
  /// Module compile states
  pub items: HashMap<String, Arc<RwLock<ErlModule>>>,
}

impl ErlCodeCache {
  const INITIAL_CODE_CACHE_SIZE: usize = 128;
}

impl Default for ErlCodeCache {
  /// Creates a new empty code cache hashmap
  fn default() -> Self {
    Self {
      items: HashMap::with_capacity(Self::INITIAL_CODE_CACHE_SIZE),
    }
  }
}
