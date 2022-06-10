//! Source file locations for printing and reporting to the user
use crate::erl_syntax::parsers::parser_input_slice::ParserInputSlice;
use std::fmt::Formatter;
use std::path::PathBuf;
use std::sync::Arc;

/// Source code span with start and end
#[derive(Clone, Debug)]
pub enum SourceLoc {
  /// We do not know the location, or do not care
  None,
  /// Points to a file
  File(PathBuf),
  /// Stores the chain of inputs, and the read position
  Input {
    /// Location in the input chain of the parser
    input: Arc<ParserInputSlice>,
  },
}

impl SourceLoc {
  /// Warns about using None but does not prevent it
  #[allow(dead_code)]
  pub(crate) fn unimplemented(file: &str, func: &str) -> Self {
    println!("Unimplemented sourceloc use {}() at {}", func, file);
    SourceLoc::None
  }

  /// Create an absolute pointer from an input position. Use this to determine source location later.
  pub(crate) fn from_input(i: Arc<ParserInputSlice>) -> Self {
    Self::Input { input: i }
  }
}

impl From<&PathBuf> for SourceLoc {
  fn from(p: &PathBuf) -> Self {
    Self::File(p.clone())
  }
}

impl std::fmt::Display for SourceLoc {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      SourceLoc::None => write!(f, "<No info>"),
      SourceLoc::Input { input } => write!(f, "{}", input),
      // SourceLoc::InputSpan { start, end } => write!(f, "Source: bytes {}..{}", start, end),
      SourceLoc::File(p) => write!(f, "File: {}", p.to_string_lossy()),
    }
  }
}
