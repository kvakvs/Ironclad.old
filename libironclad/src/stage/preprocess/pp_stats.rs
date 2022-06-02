//! Statistics and counters for preprocessor

use std::time::SystemTime;

/// Stats for caches
#[derive(Default)]
pub struct CacheStats {
  /// How many times cache request ended with a hit
  pub hits: usize,
  /// How many times the object was not in cache
  pub misses: usize,
}

/// Stores counters for preprocessor activity and start/end time
pub struct PreprocessorStats {
  /// Counter for file cache and include loader
  pub files_read: usize,
  /// Counter for bytes
  pub bytes_read: usize,
  /// Cache for input files
  pub file_cache: CacheStats,
  /// Cache for parsed input files
  pub ast_cache: CacheStats,
  /// Preprocessor run start
  pub started: SystemTime,
  /// Run end for duration
  pub finished: SystemTime,
}

impl PreprocessorStats {
  pub(crate) fn print(&self) {
    println!("Preprocessor stage stats:");
    println!("Files read: {}, bytes read: {}", self.files_read, self.bytes_read);
    println!(
      "Duration: {} microsec",
      self
        .finished
        .duration_since(self.started)
        .unwrap()
        .as_micros()
    );
  }
}

impl PreprocessorStats {
  /// Create a new stats struct
  pub fn new() -> Self {
    let time_now = SystemTime::now();
    Self {
      files_read: 0,
      bytes_read: 0,
      file_cache: CacheStats::default(),
      ast_cache: CacheStats::default(),
      started: time_now,
      finished: time_now,
    }
  }
}
