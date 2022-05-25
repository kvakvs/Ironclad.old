//! Source file locations for printing and reporting to the user
use std::fmt::Formatter;
use std::path::PathBuf;

/// Source code span with start and end
#[derive(Clone, Debug)]
pub enum SourceLoc {
  /// We do not know the location, or do not care
  None,
  /// Points to a file
  File(PathBuf),
  /// Stores a &'static str location, use the original input to determine offset/line/col etc
  InputPosition(*const u8),
  // /// A byte offset in the input source
  // InputSpan {
  //   /// Where the span starts
  //   start: usize,
  //   /// Where the span ends
  //   end: usize,
  // },
  // /// Weak pointer to a subtree of Erlang AST (for nice error reporting)
  // Ast(Weak<ErlAst>),
}

impl SourceLoc {
  /// Warns about using None but does not prevent it
  pub fn unimplemented(file: &str, func: &str) -> Self {
    println!("Unimplemented sourceloc use {}() at {}", func, file);
    SourceLoc::None
  }

  /// Create an absolute pointer from an input position. Use this to determine source location later.
  pub fn from_input(pos: &str) -> Self {
    Self::InputPosition(pos.as_ptr())
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
      SourceLoc::InputPosition(_) => write!(f, "<Input position>"),
      // SourceLoc::InputSpan { start, end } => write!(f, "Source: bytes {}..{}", start, end),
      SourceLoc::File(p) => write!(f, "File: {}", p.to_string_lossy()),
    }
  }
}
