//! Represents a loaded source file, owning its contents
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// Owns a source file text and possibly line numbers map
#[derive(Debug, Eq, PartialEq, Default)]
pub struct SourceFile {
  /// File path
  pub file_name: PathBuf,
  /// Contents of the file
  pub text: String,
  // line_numbers: Option<...>
}

impl SourceFile {
  /// Creates a new source file struct
  pub fn new(file_name: &Path, text: String) -> Arc<Self> {
    Arc::new(SourceFile {
      file_name: file_name.to_path_buf(),
      text,
    })
  }
}
