use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};

use crate::erl_error::ErlResult;
use crate::project::ErlProject;
use crate::stage::file_contents_cache::FileContentsCache;

/// Preload stage will visit all input files and load them in memory.
/// Future improvement: Lazy loading as required, timestamp checks
pub fn run(project: &mut ErlProject) -> Arc<RwLock<FileContentsCache>> {
  let mut state = FileContentsCache::new();

  for filename in &project.file_set {
    state.preload_file(filename);
  }
  println!("Read {} files, {} bytes (without include files)",
           state.contents.len(),
           state.read_bytes_count);

  Arc::new(RwLock::new(state))
}
