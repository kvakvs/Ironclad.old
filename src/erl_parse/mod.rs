use crate::project::source_file::SourceFile;

pub mod pp_parse;
mod atom;
pub mod ast;
pub mod pp_ast;
pub mod pp_directive;
pub mod erl_pp;

// /// Points to source file position and length for a text fragment.
// /// This is like &str but is not pinned to a memory address or the source string lifetime.
// /// To convert Span to a string, call Span::text() with its source file.
#[derive(Clone, Debug)]
pub struct Span {
  // pub pos: usize,
//   pub len: usize,
}

impl Span {
  pub(crate) fn text<'a>(&self, file: &'a SourceFile) -> &'a str {
    &file.text
  }
}
