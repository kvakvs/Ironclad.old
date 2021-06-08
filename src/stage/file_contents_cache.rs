use std::collections::HashMap;
use std::path::PathBuf;

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
    pub(crate) fn preload_file(&mut self, filename: &PathBuf) -> ErlResult<()> {
        let contents = std::fs::read_to_string(filename)?;
        // println!("Loaded {} - {} bytes", filename.display(), contents.len());
        self.read_bytes_count += contents.len();
        self.contents.insert(filename.clone(), contents);
        Ok(())
    }
}
