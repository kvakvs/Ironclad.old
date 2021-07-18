use std::path::{PathBuf, Path};

/// Owns a source file text and possibly line numbers map
#[derive(Debug, Eq, PartialEq)]
pub struct SourceFile {
  pub file_name: PathBuf,
  pub text: String,
  // line_numbers: Option<...>
}

impl SourceFile {
  pub fn new(file_name: &Path, text: String) -> Self {
    SourceFile {
      file_name: file_name.to_path_buf(),
      text
    }
  }
}