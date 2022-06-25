//! Represents a loaded source file, owning its contents
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// Owns a source file text and possibly line numbers map
#[derive(Debug, Eq, PartialEq, Default)]
pub struct SourceFileImpl {
  /// File path
  pub file_name: PathBuf,
  /// Contents of the file
  pub text: Arc<String>,
  // line_numbers: Option<...>
}

/// Shareable readonly source file pointer
pub type SourceFile = Arc<SourceFileImpl>;

impl SourceFileImpl {
  /// Creates a new source file struct
  pub fn new(file_name: &Path, text: String) -> SourceFile {
    SourceFileImpl {
      file_name: file_name.to_path_buf(),
      text: text.into(),
    }
    .into()
  }

  /// Create source file with text only
  pub fn no_file(text: &str) -> SourceFile {
    SourceFileImpl {
      file_name: PathBuf::new(),
      text: text.to_string().into(),
    }
    .into()
  }
}
