//! File contents cache stores all loaded files in memory
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use crate::project::source_file::SourceFile;
use crate::stage::preprocess::pp_stats::CacheStats;
use libironclad_error::ic_error::IroncladResult;
use std::sync::Arc;

/// Contains loaded files ready for parsing by the preprocessor.
/// More files will be added in preprocess stage, as include directives are parsed
pub struct FileContentsCache {
  /// Statistics of bytes read
  pub read_bytes_count: usize,
  /// File contents stored here
  pub all_files: BTreeMap<PathBuf, Arc<SourceFile>>,
}

impl Default for FileContentsCache {
  /// Create a new empty file cache
  fn default() -> Self {
    Self { read_bytes_count: 0, all_files: Default::default() }
  }
}

impl<'a> FileContentsCache {
  /// Load file contents, store entire contents in the hashmap
  pub fn preload_file(&mut self, file_name: &Path) -> IroncladResult<Arc<SourceFile>> {
    println!("Attempt to load file: {:?}", file_name);

    let contents = std::fs::read_to_string(file_name)?;
    self.read_bytes_count += contents.len();

    let src_file = SourceFile::new(file_name, contents);
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
    file_name: &Path,
  ) -> IroncladResult<Arc<SourceFile>> {
    let canon_path = file_name.canonicalize().unwrap();
    match self.all_files.get(&canon_path) {
      None => {
        cache_stats.misses += 1;
        let src_file = self.preload_file(&canon_path)?;
        Ok(src_file)
      }
      Some(contents) => Ok(contents.clone()),
    }
  }

  /// As source file text is read only, we replace.
  /// The parse trees referring the the old source file will retain their Arc<> to the old version
  pub fn update_source_text(&mut self, file_name: &Path, new_text: String) {
    let new_source_file = SourceFile::new(file_name, new_text);
    self
      .all_files
      .insert(file_name.to_path_buf(), new_source_file);
  }
}
