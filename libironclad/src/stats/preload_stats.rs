//! Statistics for file preload stage

use crate::stats::cache_stats::CacheStats;
use crate::stats::io_stats::IOStats;
use crate::stats::time_stats::TimeStats;

/// Statistics struct for file preload stage
#[derive(Default)]
pub struct FilePreloadStats {
  /// File IO counters
  pub io: IOStats,
  /// File cache statistics
  pub file_cache: CacheStats,
  /// Runtime duration counters
  pub time: TimeStats,
}

impl std::fmt::Display for FilePreloadStats {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    writeln!(f, "========================")?;
    writeln!(f, "Preload stage stats")?;
    write!(f, "{}", self.io)?;
    write!(f, "FILE {}", self.file_cache)?;
    write!(f, "{}", self.time)
  }
}
