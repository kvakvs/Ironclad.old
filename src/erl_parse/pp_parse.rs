use std::path::Path;

use nom::branch::alt;
use nom::bytes::complete::{tag, take_till, take_till1};
use nom::character::complete::{alpha1, alphanumeric1, line_ending};
use nom::combinator::{map, recognize};
use nom::multi::{many0};
use nom::sequence::{delimited, pair, terminated, tuple};

use crate::erl_error::{ErlResult};
use crate::erl_parse::{helpers};
use crate::erl_parse::helpers::ws;
use crate::erl_parse::pp_ast::{PpAstNode, PpAstTree};

/// Consume a sequence of a-zA-Z and 0-9 and underscore, which must start with not a number
///
/// Return: the word read, &str
fn parse_attr_ident(input: &str) -> nom::IResult<&str, &str> {
  recognize(pair(
    alt((alpha1, tag("_"))),
    many0(alt((alphanumeric1, tag("_")))),
  ))(input)
}

/// Parses inner value part of an attribute, until comma or closing ")", example: -attr(...)
fn parse_attr_argument(input: &str) -> nom::IResult<&str, &str> {
  fn is_comma_or_closing_paren(c: char) -> bool { c == ',' || c == ')' }
  take_till1(is_comma_or_closing_paren)(input)
}

// fn parse_attr_ident(input: &str) -> nom::IResult<&str, &str> {
//     recognize(pair(
//         alt((alpha1, tag("_"))),
//         many0(alt((alphanumeric1, tag("_")))),
//     ))(input)
// }

/// Consume bytes till the new line character, then skip the newline.
fn rest_of_the_line(input: &str) -> nom::IResult<&str, &str> {
  // terminated(anychar, line_ending)(input)
  terminated(
    take_till(|c| c == '\r' || c == '\n'),
    line_ending,
  )(input)
}

/// Consume rest of the line, skip the newline, construct a string from it
///
/// Return: Text(String) TODO: Return Span
fn parse_line(input: &str) -> nom::IResult<&str, PpAstNode> {
  map(
    rest_of_the_line,
    |s| PpAstNode::Text(String::from(s)),
  )(input)
}

/// Consume: `% ... <endline>`
fn line_comment(input: &str) -> nom::IResult<&str, PpAstNode> {
  map(
    tuple((
      tag("%"),
      rest_of_the_line,
    )),
    |(_, s)| PpAstNode::Comment(String::from(s)),
  )(input)
}

/// Consume: `- <identifier> (`
///
/// Return: identifier: &str
fn parse_attr_start_part(input: &str) -> nom::IResult<&str, &str> {
  map(
    delimited(
      ws(tag("-")),
      parse_attr_ident,
      ws(tag("(")),
    ),
    |ident| ident,
  )(input)
}

/// Parse a module attribute with 1+ parameters
fn parse_attr(input: &str) -> nom::IResult<&str, PpAstNode> {
  map(
    tuple((
      // Consume "-", "attr_name", and "("
      parse_attr_start_part,

      // Consume comma separated attr parameters till the end of the attribute
      terminated(
        take_till(|c| c == ')'),
        tuple((ws(tag(".")), line_ending)),
      ),
    )),
    |(attr_ident, body)| -> PpAstNode {
      PpAstNode::Attr { name: String::from(attr_ident), body: Some(String::from(body)) }
    },
  )(input)
}

fn parse_attr_noargs(input: &str) -> nom::IResult<&str, PpAstNode> {
  map(
    tuple((
      // Consume -attr_name(
      ws(tag("-")),
      parse_attr_ident,
      // An attribute without args ends with either (). or .
      // parentheses version is handled in parse_attr() and here we handle only "."
      ws(tag(".")),
      line_ending
    )),
    |(_, attr_ident, _tail, _newline)| -> PpAstNode {
      PpAstNode::Attr { name: String::from(attr_ident), body: None }
    },
  )(input)
}

/// Does rough preparse of ERL files, only being interested in -include, -ifdef, macros, ... etc
///
/// -define(Name(...), ...).
/// -if(Bool), -ifdef(Macro), -ifndef(Macro), -undef(Macro), -else, -elif(Bool), -endif
/// -error(Term), -warning(Term) (OTP 19+)
/// ?MODULE, ?MODULE_STRING, ?FILE, ?LINE, ?MACHINE='BEAM', ?FUNCTION_NAME, ?FUNCTION_ARITY,
/// ?OTP_RELEASE (OTP 21+)
/// ??MACRO to stringify the tokens in the macro argument
///
/// Return: Parsed preprocessor forms list (directives, and text fragments and comments)
pub fn parse_module(file_name: &Path, input: &str) -> ErlResult<PpAstTree> {
  let (tail, pp_ast) = many0(
    alt((
      line_comment,
      parse_attr,
      parse_attr_noargs,
      parse_line,
    )))(input)?;
  if !tail.is_empty() {
    return helpers::incomplete_parse_error(file_name, input, tail);
  }
  let pp_tree = PpAstTree::new(file_name.to_path_buf(), pp_ast);
  Ok(pp_tree)
}
