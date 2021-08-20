//! Code cache contains compile states in a dict
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use crate::erlang::module::ErlModule;

/// Contains cache of compile state keyed by filename
pub struct CodeCache {
  /// Module compile states
  pub items: HashMap<String, Arc<RwLock<ErlModule>>>,
}

impl CodeCache {
  const INITIAL_CODE_CACHE_SIZE: usize = 128;
}

impl Default for CodeCache {
  /// Creates a new empty code cache hashmap
  fn default() -> Self {
    Self {
      items: HashMap::with_capacity(Self::INITIAL_CODE_CACHE_SIZE)
    }
  }
}
