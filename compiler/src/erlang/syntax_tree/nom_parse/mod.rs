//! Parsers for Erlang syntax based on Nom

use std::sync::Arc;

use nom::{combinator, sequence, multi,
          bytes::complete::{tag}};

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::source_loc::SourceLoc;

pub mod parse_attr;
pub mod misc;
pub mod parse_atom;

/// Parses an attribute or a function def
pub fn parse_module_form(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  parse_attr::nom_parse_generic_attr(input)
}

/// Parses a `-module(atom).` attribute
pub fn parse_module_attr(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  combinator::map(
    sequence::tuple((
      tag("-"),
      misc::ws(tag("module")),
      tag("("),
      misc::ws(parse_atom::atom),
      tag(")"),
      misc::ws(tag(".")),
    )),
    |(_, _, _, name, _, _)| ErlAst::ModuleStartAttr {
      location: SourceLoc::None,
      name: name.to_string(),
    }.into(),
  )(input)
}

/// Parses module contents, must begin with `-module()` attr followed by 0 or more module forms.
pub fn parse_module(input: &str) -> nom::IResult<&str, Vec<Arc<ErlAst>>> {
  sequence::preceded(
    parse_module_attr,
    multi::many0(parse_module_form),
  )(input)
}

/// Given a string, try to read `-module()` attribute followed by more attributes and forms.
pub fn nom_parse_module(input: &str) -> nom::IResult<&str, Vec<Arc<ErlAst>>> {
  let (tail, forms) = parse_module(input)?;
  Ok((tail, forms))
}
