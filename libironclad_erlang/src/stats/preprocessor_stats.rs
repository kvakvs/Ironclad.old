//! Statistics and counters for preprocessor

use crate::stats::cache_stats::CacheStats;
use crate::stats::io_stats::IOStats;
use crate::stats::time_stats::TimeStats;
use std::sync::{Arc, RwLock};

/// Stores counters for preprocessor activity and start/end time
#[derive(Default)]
pub struct PreprocessorStatsImpl {
  /// Read write counters
  pub io: IOStats,
  /// Cache for input files
  pub file_cache: CacheStats,
  /// Cache for parsed input files
  pub ast_cache: CacheStats,
  /// Run duration
  pub time: TimeStats,
}

/// Wrapper for shared access
pub type PreprocessorStats = Arc<RwLock<PreprocessorStatsImpl>>;

impl std::fmt::Display for PreprocessorStatsImpl {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    writeln!(f, "========================")?;
    writeln!(f, "Preprocessor stage stats")?;
    write!(f, "{}", self.io.read().unwrap())?;
    write!(f, "FILE {}", self.file_cache.read().unwrap())?;
    write!(f, "PARSED AST {}", self.ast_cache.read().unwrap())?;
    write!(f, "{}", self.time.read().unwrap())
  }
}
