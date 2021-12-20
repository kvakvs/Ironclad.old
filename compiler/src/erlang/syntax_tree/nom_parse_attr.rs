//! Use nom parser to parse a generic module attribute from a wall of text.
use nom::{combinator, sequence, branch, multi,
          character::complete::{anychar, alpha1, alphanumeric1, line_ending, multispace0},
          bytes::complete::{tag}};
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::source_loc::SourceLoc;

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
fn ws<'a, F: 'a, O, E: nom::error::ParseError<&'a str>>(inner: F) -> impl FnMut(&'a str) -> nom::IResult<&'a str, O, E>
  where F: Fn(&'a str) -> nom::IResult<&'a str, O, E>,
{
  sequence::delimited(
    multispace0,
    inner,
    multispace0,
  )
}

/// Parse an identifier, starting with lowercase and also can be containing numbers and underscoress
fn ident(input: &str) -> nom::IResult<&str, &str> {
  combinator::recognize(
    sequence::pair(
      alpha1,
      multi::many0(branch::alt((alphanumeric1, tag("_")))),
    )
  )(input)
}

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
      ws(tag("-")),
      ws(ident),
      multi::many_till(anychar, attr_terminator)
    ))
  )(input)
}

/// Consume attribute with parentheses, from `"("` till `").\n"`
fn parenthesized_attr(input: &str) -> nom::IResult<&str, &str> {
  combinator::recognize(
    sequence::tuple((
      ws(tag("-")),
      ws(ident),
      tag("("),
      multi::many_till(anychar, parenthesized_attr_terminator)
    ))
  )(input)
}

fn generic_attr(input: &str) -> nom::IResult<&str, &str> {
  combinator::recognize(
    branch::alt((naked_attr, parenthesized_attr))
  )(input)
}

/// Given a string, try and consume a generic attribute line starting with `-ident` and ending with
/// a `"." NEWLINE`
pub fn nom_parse_generic_attr(input: &str) -> nom::IResult<&str, ErlAst> {
  let (tail, text) = generic_attr(input)?;

  let ast_node = ErlAst::UnparsedAttr {
    location: SourceLoc::None,
    text: text.to_string(),
  };
  Ok((tail, ast_node))
}