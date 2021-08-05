use std::path::PathBuf;

use pest::Span;
use std::fmt::Formatter;

#[derive(Copy, Clone, Eq, PartialEq, Default)]
pub struct SourceLoc {
  pub start: usize,
  pub end: usize,
}

impl<'i> From<pest::Span<'i>> for SourceLoc {
  fn from(sp: Span) -> Self {
    Self {
      start: sp.start(),
      end: sp.end(),
    }
  }
}

impl std::fmt::Display for SourceLoc {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "bytes {}..{}", self.start, self.end)
  }
}

/// Shows to the user where the error was found
pub struct ErrorLocation {
  /// If we know the file where this happened
  pub path: Option<PathBuf>,
  /// If we know where in file this happened
  pub location: SourceLoc,
}

impl ErrorLocation {
  /// Creates a new error location for filename and possibly AST location
  pub fn new(filename: Option<PathBuf>,
             location: SourceLoc) -> ErrorLocation {
    Self {
      path: filename,
      location,
    }
  }
}

impl std::fmt::Display for ErrorLocation {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match &self.path {
      None => write!(f, "{}", self.location),
      Some(path) => write!(f, "{}:{}", path.to_str().unwrap_or_default(), self.location),
    }
  }
}