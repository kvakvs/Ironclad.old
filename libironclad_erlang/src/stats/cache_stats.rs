//! Performance for caches statistics

use std::sync::{Arc, RwLock};

/// Stats for caches
#[derive(Default)]
pub struct CacheStatsImpl {
  /// How many times cache request ended with a hit
  pub hits: usize,
  /// How many times the object was not in cache
  pub misses: usize,
}

/// Wrapper for shared access
pub type CacheStats = Arc<RwLock<CacheStatsImpl>>;

impl std::fmt::Display for CacheStatsImpl {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    writeln!(f, "cache hits: {}, misses: {}", self.hits, self.misses)
  }
}
