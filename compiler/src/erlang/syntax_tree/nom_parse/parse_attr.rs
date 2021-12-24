//! Use nom parser to parse a generic module attribute from a wall of text.
use std::sync::Arc;
use nom::{combinator, sequence, branch, multi, character::complete::{anychar}, bytes::complete::{tag}, character};
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::nom_parse::{misc, parse_atom};
use crate::source_loc::SourceLoc;

fn attr_terminator(input: &str) -> nom::IResult<&str, &str> {
  combinator::recognize(
    sequence::pair(
      misc::ws_before(character::complete::char('.')),
      misc::newline,
    )
  )(input)
}

fn parenthesized_attr_terminator(input: &str) -> nom::IResult<&str, &str> {
  combinator::recognize(
    sequence::tuple((
      misc::ws_before(character::complete::char(')')),
      misc::ws_before(character::complete::char('.')),
      misc::newline
    ))
  )(input)
}

/// Consume attribute without parentheses, till `".\n"`
fn naked_attr(input: &str) -> nom::IResult<&str, &str> {
  combinator::recognize(
    sequence::tuple((
      misc::ws_before(character::complete::char('-')),
      misc::ws_before(misc::parse_ident),
      multi::many_till(anychar, attr_terminator)
    ))
  )(input)
}

/// Consume attribute with parentheses, from `"("` till `").\n"`
fn parenthesized_attr(input: &str) -> nom::IResult<&str, &str> {
  combinator::recognize(
    sequence::tuple((
      misc::ws_before(character::complete::char('-')),
      misc::ws_before(misc::parse_ident),
      misc::ws_before(character::complete::char('(')),
      multi::many_till(anychar, parenthesized_attr_terminator)
    ))
  )(input)
}

/// Parses a generic `- "something" ... ".\n"` attribute, consuming everything as a string
/// Given a string, try and consume a generic attribute line starting with `-ident` and ending with
/// a `"." NEWLINE`
pub fn parse_generic_attr(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  combinator::map(
    combinator::recognize(
      sequence::terminated(
        branch::alt((naked_attr, parenthesized_attr)),
        misc::newline)
    ),
    |attr| {
      let ast_node = ErlAst::UnparsedAttr {
        location: SourceLoc::None,
        text: attr.to_string(),
      };
      ast_node.into()
    })(input)
}

/// Parses a `-module(atom).` attribute
pub fn parse_module_attr(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  combinator::map(
    sequence::tuple((
      misc::ws_before(character::complete::char('-')),
      misc::ws_before(tag("module")),
      misc::ws_before(character::complete::char('(')),
      misc::ws_before(parse_atom::atom),
      misc::ws_before(character::complete::char(')')),
      attr_terminator,
    )),
    |(_, _, _, name, _, _)| ErlAst::ModuleStartAttr {
      location: SourceLoc::None,
      name: name.to_string(),
    }.into(),
  )(input)
}
