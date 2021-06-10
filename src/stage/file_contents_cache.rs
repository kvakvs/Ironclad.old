use std::collections::HashMap;
use std::path::{PathBuf, Path};

use crate::erl_error::ErlResult;
use crate::project::ErlProject;

/// Contains loaded files ready for parse by the preprocessor.
/// More files will be added in preprocess stage, as include directives are parsed
pub struct FileContentsCache {
  pub read_bytes_count: usize,
  pub contents: HashMap<PathBuf, String>,
}

impl FileContentsCache {
  pub fn new() -> Self {
    Self {
      read_bytes_count: 0,
      contents: HashMap::with_capacity(ErlProject::DEFAULT_CAPACITY),
    }
  }

  /// Load file contents, store entire contents in the hashmap
  pub(crate) fn preload_file(&mut self, file_name: &Path) -> ErlResult<()> {
    println!("Attempt to load file: {:?}", file_name);

    let contents = std::fs::read_to_string(file_name)?;
    self.read_bytes_count += contents.len();
    self.contents.insert(file_name.to_path_buf(), contents);
    Ok(())
  }

  /// Retrieve cached file contents or attempt to load (and update the cache)
  /// TODO: Cloning of strings is bad
  pub(crate) fn get_or_load(&mut self, file_name: &Path) -> ErlResult<String> {
    match self.contents.get(file_name) {
      None => {
        self.preload_file(file_name)?;
        self.get_or_load(file_name)
      }
      Some(contents) => Ok(contents.clone())
    }
  }
}
