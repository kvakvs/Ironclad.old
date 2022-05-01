//! Source file locations for printing and reporting to the user
use std::path::PathBuf;
use std::fmt::Formatter;
use std::sync::Weak;
use crate::erlang::syntax_tree::erl_ast::ErlAst;

/// Source code span with start and end
#[derive(Clone, Debug)]
pub enum SourceLoc {
  /// We do not know the location, or do not care
  None,
  /// Points to a file
  File(PathBuf),
  /// A byte offset in the input source
  Span {
    /// Where the span starts
    start: usize,
    /// Where the span ends
    end: usize,
  },
  /// Weak pointer to a subtree of Erlang AST (for nice error reporting)
  Ast(Weak<ErlAst>),
}

impl From<&PathBuf> for SourceLoc {
  fn from(p: &PathBuf) -> Self {
    Self::File(p.clone())
  }
}

impl std::fmt::Display for SourceLoc {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      SourceLoc::None => write!(f, "Source:?"),
      SourceLoc::Span { start, end } => write!(f, "Source: bytes {}..{}", start, end),
      SourceLoc::Ast(ast) => write!(f, "Source:{}", ast.upgrade().unwrap()),
      SourceLoc::File(p) => write!(f, "File: {}", p.to_string_lossy())
    }
  }
}

// /// Shows to the user where the error was found
// pub struct ErrorLocation {
//   /// If we know the file where this happened
//   pub path: Option<PathBuf>,
//   /// If we know where in file this happened
//   pub location: SourceLoc,
// }
//
// impl ErrorLocation {
//   /// Creates a new error location for filename and possibly AST location
//   pub fn new(filename: Option<PathBuf>,
//              location: SourceLoc) -> SourceLoc {
//     Self {
//       path: filename,
//       location,
//     }
//   }
//
//   /// Create an empty non-location
//   pub fn empty() -> SourceLoc {
//     Self {
//       path: None,
//       location: SourceLoc::None,
//     }
//   }
// }
//
// impl std::fmt::Display for ErrorLocation {
//   fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//     match &self.path {
//       None => write!(f, "{}", self.location),
//       Some(path) => write!(f, "{}:{}", path.to_str().unwrap_or_default(), self.location),
//     }
//   }
// }