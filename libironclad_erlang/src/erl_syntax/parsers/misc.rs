//! Helper functions for Nom parsing

use crate::erl_syntax::parsers::{StrSliceParserResult, StringParserResult};
use nom::branch::alt;
use nom::combinator::{eof, map, not, opt, peek, recognize, verify};
use nom::error::convert_error;
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
    ws_before_mut(recognize(pair(
      verify(character::complete::anychar, |c: &char| c.is_alphabetic() && c.is_lowercase()),
      many0(alt((alphanumeric1, tag("_")))),
    ))),
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
  recognize(preceded(
    many0(alt((char(' '), char('\t')))),
    alt((tag("\r\n"), tag("\r"), tag("\n"), eof)),
  ))(input)
}

/// Matches an opening parenthesis "(" with 0+ whitespace before
#[inline]
pub fn par_open<'a, ErrType: 'a + nom::error::ParseError<&'a str>>(
  input: &'a str,
) -> nom::IResult<&str, &str, ErrType> {
  recognize(ws_before(char('(')))(input)
}

/// Matches a closing parenthesis ")" with 0+ whitespace before
#[inline]
pub fn par_close<'a, ErrType: 'a + nom::error::ParseError<&'a str>>(
  input: &'a str,
) -> nom::IResult<&str, &str, ErrType> {
  recognize(ws_before(char(')')))(input)
}

/// Matches an opening curly bracket "{" with 0+ whitespace before
#[inline]
pub fn curly_open<'a, ErrType: 'a + nom::error::ParseError<&'a str>>(
  input: &'a str,
) -> nom::IResult<&str, &str, ErrType> {
  recognize(ws_before(char('{')))(input)
}

/// Matches a closing curly bracket "}" with 0+ whitespace before
#[inline]
pub fn curly_close<'a, ErrType: 'a + nom::error::ParseError<&'a str>>(
  input: &'a str,
) -> nom::IResult<&str, &str, ErrType> {
  recognize(ws_before(char('}')))(input)
}

/// Matches an opening square bracket "[" with 0+ whitespace before
#[inline]
pub fn square_open<'a, ErrType: 'a + nom::error::ParseError<&'a str>>(
  input: &'a str,
) -> nom::IResult<&str, &str, ErrType> {
  recognize(ws_before(char('[')))(input)
}

/// Matches a closing square bracket "]" with 0+ whitespace before
#[inline]
pub fn square_close<'a, ErrType: 'a + nom::error::ParseError<&'a str>>(
  input: &'a str,
) -> nom::IResult<&str, &str, ErrType> {
  recognize(ws_before(char(']')))(input)
}

/// Matches a comma "," with 0+ whitespace before
#[inline]
pub fn comma<'a, ErrType: 'a + nom::error::ParseError<&'a str>>(
  input: &'a str,
) -> nom::IResult<&str, &str, ErrType> {
  recognize(ws_before(char(',')))(input)
}

/// Matches a hash symbol `"#"` with 0+ whitespace before
#[inline]
pub fn hash_symbol<'a, ErrType: 'a + nom::error::ParseError<&'a str>>(
  input: &'a str,
) -> nom::IResult<&str, &str, ErrType> {
  recognize(ws_before(char('#')))(input)
}

/// Matches a period "." with 0+ whitespace before
#[inline]
pub fn period<'a, ErrType: 'a + nom::error::ParseError<&'a str>>(
  input: &'a str,
) -> nom::IResult<&str, &str, ErrType> {
  recognize(ws_before(char('.')))(input)
}

/// Recognizes end of a directive or module attribute in `-<attr> ... "." <newline>`
#[inline]
pub fn period_newline<'a, ErrType: 'a + nom::error::ParseError<&'a str>>(
  input: &'a str,
) -> nom::IResult<&str, &str, ErrType> {
  recognize(pair(period, newline_or_eof))(input)
}

/// Matches a semicolon ";" with 0+ whitespace before
#[inline]
pub fn semicolon<'a, ErrType: 'a + nom::error::ParseError<&'a str>>(
  input: &'a str,
) -> nom::IResult<&str, &str, ErrType> {
  recognize(ws_before(char(';')))(input)
}

/// Matches a double colon "::" with 0+ whitespace before
#[inline]
pub fn colon_colon<'a, ErrType: 'a + nom::error::ParseError<&'a str>>(
  input: &'a str,
) -> nom::IResult<&str, &str, ErrType> {
  recognize(ws_before(tag("::")))(input)
}

/// Matches an equals sign "=" with 0+ whitespace before
#[inline]
pub fn equals_sign<'a, ErrType: 'a + nom::error::ParseError<&'a str>>(
  input: &'a str,
) -> nom::IResult<&str, &str, ErrType> {
  recognize(ws_before(char('=')))(input)
}

/// Recognizes `% text <newline>` consuming text
pub fn parse_line_comment<'a, ErrType: nom::error::ParseError<&'a str>>(
  input: &'a str,
) -> nom::IResult<&str, &str, ErrType> {
  recognize(pair(many1(char('%')), many_till(character::complete::anychar, newline_or_eof)))(input)
}

/// Print detailed error with source pointers, and panic
pub fn panicking_parser_error_reporter<'a, Out>(
  input: &'a str,
  res: Result<(&'a str, Out), nom::error::VerboseError<&'a str>>,
) -> (&'a str, Out) {
  match res {
    Ok((tail, out)) => {
      let tail_trim_whitespace = tail.trim();

      if !tail_trim_whitespace.is_empty() {
        panic!("Not all input was consumed: tail=«{}»", tail_trim_whitespace)
      }
      (tail, out)
    }
    Err(e) => panic!("Nom parser error: {}", convert_error(input, e)),
  }
}

/// Returns a new parser which recognizes a `<spaces> "-" <spaces> <tag>` and returns it as
/// a `&str` slice (*recognizes*, i.e. returns with all whitespace included)
pub fn match_dash_tag<'a, ErrType: 'a + nom::error::ParseError<&'a str>>(
  tag_str: &'static str,
) -> impl FnMut(&'a str) -> nom::IResult<&'a str, &'a str, ErrType> {
  recognize(pair(ws_before(char('-')), match_word(tag_str)))
}

/// Matches a non-letter, use with `peek` to mark where word ends
pub fn word_break<'a, ErrType: 'a + nom::error::ParseError<&'a str>>(
  input: &'a str,
) -> nom::IResult<&str, &str, ErrType> {
  recognize(not(alphanumeric1))(input)
}

/// Matches a tag which is followed by a non-letter (word break)
pub fn match_word<'a, ErrType: 'a + nom::error::ParseError<&'a str>>(
  tag_str: &'static str,
) -> impl FnMut(&'a str) -> nom::IResult<&'a str, &'a str, ErrType> {
  recognize(terminated(ws_before(tag(tag_str)), peek(word_break)))
}
