//! Source file locations for printing and reporting to the user
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::parsers::parser_input_slice::ParserInputSlice;
use crate::erl_syntax::token_stream::token::Token;
use std::fmt::Formatter;
use std::path::PathBuf;
use std::sync::Arc;

/// Source code span with start and end
#[derive(Clone, Debug)]
pub enum SourceLoc {
  /// We do not know the location, or do not care
  None,
  /// Offset in the input string
  Offset {
    /// Start of the input
    start: *const u8,
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
  pub(crate) fn new(input: &ParserInput) -> Self {
    Self::Offset { start: input.tokens.iter().next().unwrap().offset }
  }
}

impl std::fmt::Display for SourceLoc {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      SourceLoc::None => write!(f, "<No info>"),
      SourceLoc::Offset { start } => write!(f, "SourceLoc[{:x}]", *start as usize),
    }
  }
}
