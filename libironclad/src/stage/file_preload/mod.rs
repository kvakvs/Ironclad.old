//! Scans the project directory structure and preloads all ERL and HRL source files into memory

use crate::stage::file_contents_cache::FileContentsCache;
use libironclad_error::ic_error::IroncladResult;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};
use std::time::SystemTime;

/// Statistics for file preload stage
pub struct FilePreloadStats {
  /// Files read count
  files_read: usize,
  /// Read bytes count
  bytes_read: usize,
  /// Start time for benchmark
  started: SystemTime,
  /// Finish time
  finished: SystemTime,
}

impl FilePreloadStats {
  /// Create new stats struct
  pub fn new() -> Self {
    let time_now = SystemTime::now();
    Self {
      files_read: 0,
      bytes_read: 0,
      started: time_now,
      finished: time_now,
    }
  }
}

/// Handles loading/caching text files in memory
pub struct FilePreloadStage {
  /// Counters and timers for performance profiling
  pub stats: FilePreloadStats,
}

impl FilePreloadStage {
  /// Creates new state for file preload stage
  pub fn new() -> Self {
    Self { stats: FilePreloadStats::new() }
  }

  /// Stage 0 - Preloading sources
  /// ----------------------------
  /// Preload stage will visit all input files and load them in memory.
  /// Future improvement: Lazy loading as required, timestamp checks
  pub fn run(&mut self, inputs: &[PathBuf]) -> IroncladResult<Arc<RwLock<FileContentsCache>>> {
    let mut state = FileContentsCache::default();

    for filename in inputs {
      state.preload_file(filename)?;
    }
    println!(
      "Read {} files, {} bytes (without include files)",
      state.all_files.len(),
      state.read_bytes_count
    );

    Ok(RwLock::new(state).into())
  }
}
