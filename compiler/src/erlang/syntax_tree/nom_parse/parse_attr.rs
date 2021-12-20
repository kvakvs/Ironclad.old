//! Use nom parser to parse a generic module attribute from a wall of text.
use std::sync::Arc;
use nom::{combinator, sequence, branch, multi,
          character::complete::{anychar, line_ending},
          bytes::complete::{tag}};
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::nom_parse::misc;
use crate::source_loc::SourceLoc;

fn attr_terminator(input: &str) -> nom::IResult<&str, &str> {
  combinator::recognize(
    sequence::tuple((
      tag("."),
      line_ending
    ))
  )(input)
}

fn parenthesized_attr_terminator(input: &str) -> nom::IResult<&str, &str> {
  combinator::recognize(
    sequence::tuple((
      tag(")"),
      tag("."),
      line_ending
    ))
  )(input)
}

/// Consume attribute without parentheses, till `".\n"`
fn naked_attr(input: &str) -> nom::IResult<&str, &str> {
  combinator::recognize(
    sequence::tuple((
      misc::ws(tag("-")),
      misc::ws(misc::ident),
      multi::many_till(anychar, attr_terminator)
    ))
  )(input)
}

/// Consume attribute with parentheses, from `"("` till `").\n"`
fn parenthesized_attr(input: &str) -> nom::IResult<&str, &str> {
  combinator::recognize(
    sequence::tuple((
      misc::ws(tag("-")),
      misc::ws(misc::ident),
      tag("("),
      multi::many_till(anychar, parenthesized_attr_terminator)
    ))
  )(input)
}

/// Parses a generic `- "something" ... ".\n"` attribute, consuming everything as a string
pub fn generic_attr(input: &str) -> nom::IResult<&str, &str> {
  combinator::recognize(
    branch::alt((naked_attr, parenthesized_attr))
  )(input)
}

/// Given a string, try and consume a generic attribute line starting with `-ident` and ending with
/// a `"." NEWLINE`
pub fn nom_parse_generic_attr(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  let (tail, text) = generic_attr(input)?;

  let ast_node = ErlAst::UnparsedAttr {
    location: SourceLoc::None,
    text: text.to_string(),
  };
  Ok((tail, ast_node.into()))
}