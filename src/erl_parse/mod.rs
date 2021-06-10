use crate::erl_module::ErlModule;
use crate::erl_parse::ast::ASTNode;
use crate::erl_parse::atom::parse_atom;
use crate::erl_parse::helpers::ws;

mod helpers;
pub mod pp_parse;
mod atom;
pub mod ast;
pub mod pp_ast;

/// Points to source file position and length for a text fragment.
/// This is like &str but is not pinned to a memory address or the source string lifetime.
#[derive(Debug)]
pub struct Span {
  pub pos: usize,
  pub len: usize,
}

impl Span {
  pub fn new(pos: usize, len: usize) -> Self {
    Self { pos, len }
  }
}

// fn parse_module_attribute(i: &str) -> nom::IResult<String, AST> {
//     nom::sequence::tuple((
//         ws(nom::bytes::complete::tag("-module")),
//         parse_atom,
//         ws(nom::bytes::complete::tag(")")),
//     ))(i)
// }

pub fn parse_module(i: &str) -> nom::IResult<&str, Vec<ASTNode>> {
  // let forms = nom::branch::alt((
  //     // parse_module_attribute,
  //     // parse_function,
  // ))(i);

  // ErlModule::from_forms(forms)
  Ok(("", vec![]))
}

// pub fn parse_test() {
//     let input = "-module(fgsfds).\n\
//     myfun(Args) -> ok.\n\
//     ";
//     println!("{:?}", parse_module(input));
// }
