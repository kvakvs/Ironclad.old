//! Scans the project directory structure and preloads all ERL and HRL source files into memory

use crate::erl_error::ErlResult;
use crate::project::ErlProject;
use crate::stage::file_contents_cache::FileContentsCache;
use std::sync::{Arc, Mutex};

/// Handles loading/caching text files in memory
pub struct FilePreloadStage {}

impl FilePreloadStage {
  /// Stage 0 - Preloading sources
  /// ----------------------------
  /// Preload stage will visit all input files and load them in memory.
  /// Future improvement: Lazy loading as required, timestamp checks
  pub fn run(project: &mut ErlProject) -> ErlResult<Arc<Mutex<FileContentsCache>>> {
    let mut state = FileContentsCache::default();

    for filename in &project.inputs {
      state.preload_file(filename)?
    }
    println!("Read {} files, {} bytes (without include files)",
             state.all_files.len(),
             state.read_bytes_count);

    Ok(Arc::new(Mutex::new(state)))
  }
}
