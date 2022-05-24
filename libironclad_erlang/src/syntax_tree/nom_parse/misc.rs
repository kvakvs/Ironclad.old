//! Helper functions for Nom parsing

use crate::syntax_tree::nom_parse::{StrSliceParserResult, StringParserResult};
use nom::branch::alt;
use nom::combinator::{eof, map, opt, recognize, verify};
use nom::multi::{many0, many1, many_till};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::{
  bytes::complete::tag,
  character,
  character::complete::{alphanumeric1, char, one_of},
};

/// Recognizes 0 or more whitespaces and line comments
fn spaces_or_comments0<'a, ErrType: nom::error::ParseError<&'a str>>(
  input: &'a str,
) -> nom::IResult<&str, &str, ErrType> {
  recognize(many0(alt((character::complete::multispace1, parse_line_comment))))(input)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes leading
/// whitespace, returning the output of `inner`.
pub fn ws_before<'a, InnerFn: 'a, Out, ErrType: nom::error::ParseError<&'a str>>(
  inner: InnerFn,
) -> impl FnMut(&'a str) -> nom::IResult<&'a str, Out, ErrType>
where
  InnerFn: Fn(&'a str) -> nom::IResult<&'a str, Out, ErrType>,
{
  preceded::<&'a str, &str, Out, ErrType, _, InnerFn>(spaces_or_comments0, inner)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes leading
/// whitespace, returning the output of `inner`.
pub fn ws_before_mut<'a, InnerFn: 'a, Out, ErrType: nom::error::ParseError<&'a str>>(
  inner: InnerFn,
) -> impl FnMut(&'a str) -> nom::IResult<&'a str, Out, ErrType>
where
  InnerFn: FnMut(&'a str) -> nom::IResult<&'a str, Out, ErrType>,
{
  preceded(spaces_or_comments0, inner)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
pub fn ws<'a, InnerFn: 'a, Out, ErrType: nom::error::ParseError<&'a str>>(
  inner: InnerFn,
) -> impl FnMut(&'a str) -> nom::IResult<&'a str, Out, ErrType>
where
  InnerFn: Fn(&'a str) -> nom::IResult<&'a str, Out, ErrType>,
{
  delimited(spaces_or_comments0, inner, spaces_or_comments0)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
pub fn ws_mut<'a, InnerFn: 'a, Out, ErrType: nom::error::ParseError<&'a str>>(
  inner: InnerFn,
) -> impl FnMut(&'a str) -> nom::IResult<&'a str, Out, ErrType>
where
  InnerFn: FnMut(&'a str) -> nom::IResult<&'a str, Out, ErrType>,
{
  delimited(spaces_or_comments0, inner, character::complete::multispace0)
}

/// Parse an identifier, starting with lowercase and also can be containing numbers and underscoress
pub fn parse_ident(input: &str) -> StringParserResult {
  map(
    recognize(pair(
      verify(character::complete::anychar, |c: &char| c.is_alphabetic() && c.is_lowercase()),
      many0(alt((alphanumeric1, tag("_")))),
    )),
    |result: &str| result.to_string(),
  )(input)
}

/// Parse an identifier, starting with lowercase and also can be containing numbers and underscoress
pub fn parse_varname(input: &str) -> StringParserResult {
  map(
    recognize(pair(
      // a variable is a pair of UPPERCASE or _, followed by any alphanum or _
      verify(character::complete::anychar, |c: &char| c.is_uppercase() || *c == '_'),
      many0(alt((alphanumeric1, tag("_")))),
    )),
    |result: &str| result.to_string(),
  )(input)
}

/// Parse an integer without a sign. Signs apply as unary operators. Output is a string.
/// From Nom examples
pub fn parse_int(input: &str) -> StrSliceParserResult {
  recognize(many1(terminated(one_of("0123456789"), many0(char('_')))))(input)
}

/// Parse a float with possibly scientific notation. Output is a string.
/// From Nom examples
pub fn parse_float(input: &str) -> StrSliceParserResult {
  alt((
    // Case one: .42
    recognize(tuple((
      char('.'),
      parse_int,
      opt(tuple((one_of("eE"), opt(one_of("+-")), parse_int))),
    ))),
    // Case two: 42e42 and 42.42e42
    recognize(tuple((
      parse_int,
      opt(preceded(char('.'), parse_int)),
      one_of("eE"),
      opt(one_of("+-")),
      parse_int,
    ))),
    // Case three: 42. (disallowed because end of function is also period) and 42.42
    recognize(tuple((parse_int, char('.'), parse_int))),
  ))(input)
}

// /// Matches newline \n or \r\n
// pub fn newline(input: &str) -> nom::IResult<&str, (), ErlParserError> {
//   map(
//     alt((
//       eof,
//       Self::ws(tag("\r\n")),
//       Self::ws(tag("\n"))
//     )),
//     |_| (),
//   )(input)
// }

/// Recognizes newline or end of input
pub fn newline_or_eof<'a, ErrType: nom::error::ParseError<&'a str>>(
  input: &'a str,
) -> nom::IResult<&str, &str, ErrType> {
  recognize(alt((tag("\r\n"), tag("\r"), tag("\n"), eof)))(input)
}

/// Recognizes `% text <newline>` consuming text
pub fn parse_line_comment<'a, ErrType: nom::error::ParseError<&'a str>>(
  input: &'a str,
) -> nom::IResult<&str, &str, ErrType> {
  recognize(pair(many1(char('%')), many_till(character::complete::anychar, newline_or_eof)))(input)
}
