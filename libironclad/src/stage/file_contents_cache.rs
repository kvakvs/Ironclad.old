//! File contents cache stores all loaded files in memory
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use crate::stats::cache_stats::CacheStats;
use crate::stats::io_stats::IOStats;
use libironclad_erlang::error::ic_error::IroncladResult;
use libironclad_erlang::source_file::SourceFileImpl;
use std::sync::Arc;

/// Contains loaded files ready for parsing by the preprocessor.
/// More files will be added in preprocess stage, as include directives are parsed
pub struct FileContentsCache {
  /// File contents stored here
  pub all_files: BTreeMap<PathBuf, Arc<SourceFileImpl>>,
}

impl Default for FileContentsCache {
  /// Create a new empty file cache
  fn default() -> Self {
    Self { all_files: Default::default() }
  }
}

impl FileContentsCache {
  /// Load file contents, store entire contents in the hashmap
  pub fn preload_file(
    &mut self,
    io_stats: &mut IOStats,
    file_name: &Path,
  ) -> IroncladResult<Arc<SourceFileImpl>> {
    println!("Attempt to load file: {:?}", file_name);

    let contents = std::fs::read_to_string(file_name)?;
    io_stats.files_read += 1;
    io_stats.bytes_read += contents.len();

    let src_file = SourceFileImpl::new(file_name, contents);
    self
      .all_files
      .insert(file_name.to_path_buf(), src_file.clone());
    Ok(src_file)
  }

  /// Retrieve cached file contents or attempt to load (and update the cache)
  /// TODO: Cloning of strings is bad
  pub(crate) fn get_or_load(
    &mut self,
    cache_stats: &mut CacheStats,
    io_stats: &mut IOStats,
    file_name: &Path,
  ) -> IroncladResult<Arc<SourceFileImpl>> {
    let canon_path = file_name.canonicalize().unwrap();

    match self.all_files.get(&canon_path) {
      None => {
        cache_stats.misses += 1;
        let src_file = self.preload_file(io_stats, &canon_path)?;
        Ok(src_file)
      }

      Some(contents) => {
        cache_stats.hits += 1;
        Ok(contents.clone())
      }
    }
  }

  /// As source file text is read only, we replace.
  /// The parse trees referring the the old source file will retain their Arc<> to the old version
  pub fn update_source_text(&mut self, file_name: &Path, new_text: String) {
    let new_source_file = SourceFileImpl::new(file_name, new_text);
    self
      .all_files
      .insert(file_name.to_path_buf(), new_source_file);
  }
}
