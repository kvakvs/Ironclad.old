use std::collections::HashMap;
use std::path::{PathBuf, Path};

use crate::erl_error::ErlResult;
use crate::project::ErlProject;
use crate::project::source_file::SourceFile;
use std::sync::Arc;

/// Contains loaded files ready for s2_parse by the preprocessor.
/// More files will be added in s1_preprocess stage, as include directives are parsed
pub struct FileContentsCache {
  pub read_bytes_count: usize,
  pub all_files: HashMap<PathBuf, Arc<SourceFile>>,
}

impl<'a> FileContentsCache {
  pub fn new() -> Self {
    Self {
      read_bytes_count: 0,
      all_files: HashMap::with_capacity(ErlProject::DEFAULT_CAPACITY),
    }
  }

  /// Load file contents, store entire contents in the hashmap
  pub(crate) fn preload_file(&mut self, file_name: &Path) -> ErlResult<()> {
    println!("Attempt to load file: {:?}", file_name);

    let contents = std::fs::read_to_string(file_name)?;
    self.read_bytes_count += contents.len();

    let src_file_definition = Arc::new(SourceFile::new(file_name, contents));
    self.all_files.insert(file_name.to_path_buf(), src_file_definition);
    Ok(())
  }

  /// Retrieve cached file contents or attempt to load (and update the cache)
  /// TODO: Cloning of strings is bad
  pub(crate) fn get_or_load(&mut self, file_name: &Path) -> ErlResult<Arc<SourceFile>> {
    match self.all_files.get(file_name) {
      None => {
        self.preload_file(file_name)?;
        self.get_or_load(file_name)
      }
      Some(contents) => Ok(contents.clone())
    }
  }

  /// As source file text is read only, we replace.
  /// The s2_parse trees referring the the old source file will retain their Arc<> to the old version
  pub fn update_source_text(&mut self, file_name: &Path, new_text: String) {
    let new_source_file = Arc::new(SourceFile::new(file_name, new_text));
    self.all_files.insert(file_name.to_path_buf(), new_source_file);
  }
}
