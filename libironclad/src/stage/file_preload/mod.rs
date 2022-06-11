//! Scans the project directory structure and preloads all ERL and HRL source files into memory

use libironclad_erlang::error::ic_error::IroncladResult;
use libironclad_erlang::file_cache::FileCache;
use libironclad_erlang::stats::preload_stats::FilePreloadStats;
use std::path::PathBuf;

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
  pub fn run(&mut self, inputs: &[PathBuf]) -> IroncladResult<FileCache> {
    let f_cache = FileCache::default();

    for filename in inputs {
      if let Ok(mut w_f_cache) = f_cache.write() {
        w_f_cache.preload_file(filename)?;
      } else {
        panic!("Can't lock file cache for updating")
      }
    }

    // Print runtime and counters
    if let Ok(r_stats) = self.stats.read() {
      if let Ok(mut w_time) = r_stats.time.write() {
        w_time.stage_finished(); // mark ending time
      } else {
        panic!("Can't lock time stats for updating")
      }
      print!("{}", r_stats);
    } else {
      panic!("Can't lock self.stats for read")
    }

    Ok(f_cache)
  }
}
