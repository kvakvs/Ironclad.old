//! Scans the project directory structure and preloads all ERL and HRL source files into memory

use std::path::PathBuf;
use crate::erl_error::ErlResult;
use crate::stage::file_contents_cache::FileContentsCache;
use std::sync::{Arc, RwLock};

/// Handles loading/caching text files in memory
pub struct FilePreloadStage {}

impl FilePreloadStage {
  /// Stage 0 - Preloading sources
  /// ----------------------------
  /// Preload stage will visit all input files and load them in memory.
  /// Future improvement: Lazy loading as required, timestamp checks
  pub fn run(inputs: &[PathBuf]) -> ErlResult<Arc<RwLock<FileContentsCache>>> {
    let mut state = FileContentsCache::default();

    for filename in inputs {
      state.preload_file(filename)?
    }
    println!("Read {} files, {} bytes (without include files)",
             state.all_files.len(),
             state.read_bytes_count);

    Ok(RwLock::new(state).into())
  }
}
