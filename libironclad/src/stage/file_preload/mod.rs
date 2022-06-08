//! Scans the project directory structure and preloads all ERL and HRL source files into memory

use crate::stage::file_contents_cache::FileContentsCache;
use crate::stats::preload_stats::FilePreloadStats;
use libironclad_erlang::error::ic_error::IroncladResult;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};

/// Handles loading/caching text files in memory
#[derive(Default)]
pub struct FilePreloadStage {
  /// Counters and timers for performance profiling
  pub stats: FilePreloadStats,
}

impl FilePreloadStage {
  /// Stage 0 - Preloading sources
  /// ----------------------------
  /// Preload stage will visit all input files and load them in memory.
  /// Future improvement: Lazy loading as required, timestamp checks
  pub fn run(&mut self, inputs: &[PathBuf]) -> IroncladResult<Arc<RwLock<FileContentsCache>>> {
    let mut f_cache = FileContentsCache::default();

    for filename in inputs {
      f_cache.preload_file(&mut self.stats.io, filename)?;
    }

    // Print runtime and counters
    self.stats.time.stage_finished(); // mark ending time
    print!("{}", self.stats);

    Ok(RwLock::new(f_cache).into())
  }
}
