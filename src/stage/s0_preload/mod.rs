use crate::erl_error::ErlResult;
use crate::project::ErlProject;
use crate::stage::file_contents_cache::FileContentsCache;
use std::sync::{Arc, Mutex};

/// Preload stage will visit all input files and load them in memory.
/// Future improvement: Lazy loading as required, timestamp checks
pub fn run(project: &mut ErlProject) -> ErlResult<Arc<Mutex<FileContentsCache>>> {
  let mut state = FileContentsCache::new();

  for filename in &project.file_set {
    state.preload_file(filename)?
  }
  println!("Read {} files, {} bytes (without include files)",
           state.all_files.len(),
           state.read_bytes_count);

  Ok(Arc::new(Mutex::new(state)))
}
