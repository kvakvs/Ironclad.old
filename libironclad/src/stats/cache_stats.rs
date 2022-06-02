//! Performance for caches statistics

/// Stats for caches
#[derive(Default)]
pub struct CacheStats {
  /// How many times cache request ended with a hit
  pub hits: usize,
  /// How many times the object was not in cache
  pub misses: usize,
}

impl std::fmt::Display for CacheStats {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    writeln!(f, "cache hits: {}, misses: {}", self.hits, self.misses)
  }
}
