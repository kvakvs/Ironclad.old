//! Statistics and counters for preprocessor

use crate::stats::cache_stats::CacheStats;
use crate::stats::io_stats::IOStats;
use crate::stats::time_stats::TimeStats;

/// Stores counters for preprocessor activity and start/end time
pub struct PreprocessorStats {
  /// Read write counters
  pub io: IOStats,
  /// Cache for input files
  pub file_cache: CacheStats,
  /// Cache for parsed input files
  pub ast_cache: CacheStats,
  /// Run duration
  pub time: TimeStats,
}

impl std::fmt::Display for PreprocessorStats {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    writeln!(f, "========================")?;
    writeln!(f, "Preprocessor stage stats")?;
    write!(f, "{}", self.io)?;
    write!(f, "FILE {}", self.file_cache)?;
    write!(f, "PARSED AST {}", self.ast_cache)?;
    write!(f, "{}", self.time)
  }
}

impl PreprocessorStats {
  /// Create a new stats struct
  pub fn new() -> Self {
    Self {
      io: IOStats::default(),
      file_cache: CacheStats::default(),
      ast_cache: CacheStats::default(),
      time: TimeStats::default(),
    }
  }
}
