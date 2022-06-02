//! File IO stat counters

/// Counts files and bytes read and written
pub struct IOStats {
  /// Files read count
  pub files_read: usize,
  /// Read bytes count
  pub bytes_read: usize,
  /// Files written count
  pub files_written: usize,
  /// Bytes written count
  pub bytes_written: usize,
}

impl std::fmt::Display for IOStats {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "READ files: {}, bytes: {}; ", self.files_read, self.bytes_read)?;
    writeln!(f, "WRITE files: {}, bytes: {}", self.files_written, self.bytes_written)
  }
}

impl Default for IOStats {
  fn default() -> Self {
    Self {
      files_read: 0,
      bytes_read: 0,
      files_written: 0,
      bytes_written: 0,
    }
  }
}
