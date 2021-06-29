use crate::project::source_file::SourceFile;

mod helpers;
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
  //   assert!(self.pos < file.text.len(), "Position is beyond source length");
  //   assert!(self.pos + self.len < file.text.len(), "Position+Length are beyond source length");
  //   &file.text[self.pos..(self.pos + self.len)]
    &file.text
  }

  // pub fn new(pos: usize, len: usize) -> Self {
  //   Self { pos, len }
  // }

  // pub fn from_str(source_file: &SourceFile, slice: &str) -> Self {
  //   Self { pos: source_file.text.offset(slice), len: slice.len() }
  // }
}

// fn parse_module_attribute(i: &str) -> nom::IResult<String, AST> {
//     nom::sequence::tuple((
//         ws(nom::bytes::complete::tag("-module")),
//         parse_atom,
//         ws(nom::bytes::complete::tag(")")),
//     ))(i)
// }

// pub fn parse_module<'a>(_source_file: Arc<SourceFile>) -> nom::IResult<&'a str, Vec<ErlAstNode>> {
//   // let forms = nom::branch::alt((
//   //     // parse_module_attribute,
//   //     // parse_function,
//   // ))(i);
//
//   // ErlModule::from_forms(forms)
//   Ok(("", vec![]))
// }

// pub fn parse_test() {
//     let input = "-module(fgsfds).\n\
//     myfun(Args) -> ok.\n\
//     ";
//     println!("{:?}", parse_module(input));
// }
