//! File IO stat counters

use std::sync::{Arc, RwLock};

/// Counts files and bytes read and written
#[derive(Debug, Default)]
pub struct IOStatsImpl {
  /// Files read count
  pub files_read: usize,
  /// Read bytes count
  pub bytes_read: usize,
  /// Files written count
  pub files_written: usize,
  /// Bytes written count
  pub bytes_written: usize,
}

/// Wrapper for shared access
pub type IOStats = Arc<RwLock<IOStatsImpl>>;

impl std::fmt::Display for IOStatsImpl {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "READ files: {}, bytes: {}; ", self.files_read, self.bytes_read)?;
    writeln!(f, "WRITE files: {}, bytes: {}", self.files_written, self.bytes_written)
  }
}
