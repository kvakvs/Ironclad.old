//! Statistics for file preload stage

use crate::stats::cache_stats::CacheStats;
use crate::stats::io_stats::IOStats;
use crate::stats::time_stats::TimeStats;
use std::sync::{Arc, RwLock};

/// Statistics struct for file preload stage
#[derive(Default)]
pub struct FilePreloadStatsImpl {
  /// File IO counters
  pub io: IOStats,
  /// File cache statistics
  pub file_cache: CacheStats,
  /// Runtime duration counters
  pub time: TimeStats,
}

/// Wrapper for shared access
pub type FilePreloadStats = Arc<RwLock<FilePreloadStatsImpl>>;

impl std::fmt::Display for FilePreloadStatsImpl {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    writeln!(f, "========================")?;
    writeln!(f, "Preload stage stats")?;
    self.io.read().unwrap().fmt(f)?;
    write!(f, "FILE {}", self.file_cache.read().unwrap())?;
    self.time.read().unwrap().fmt(f)
  }
}
