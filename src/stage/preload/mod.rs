use crate::erl_error::ErlResult;
use crate::project::ErlProject;
use crate::stage::file_contents_cache::FileContentsCache;
use crate::types::{ArcRw, create_arcrw};

/// Preload stage will visit all input files and load them in memory.
/// Future improvement: Lazy loading as required, timestamp checks
pub fn run(project: &mut ErlProject) -> ErlResult<ArcRw<FileContentsCache>> {
  let mut state = FileContentsCache::new();

  for filename in &project.file_set {
    state.preload_file(filename)?
  }
  println!("Read {} files, {} bytes (without include files)",
           state.contents.len(),
           state.read_bytes_count);

  Ok(create_arcrw(state))
}
