use std::path::Path;

use nom::character::complete::{multispace0, multispace1};
use nom::error::{FromExternalError, ParseError};
use nom::IResult;
use nom::multi::fold_many0;
use nom::sequence::{delimited, preceded};

use crate::erl_error::{ErlResult, ErrorLocation};
use crate::erl_error::ErlError::ErlParseError;
use crate::erl_parse::Span;
use nom::branch::alt;
use nom::combinator::{map, value, verify, map_res, map_opt};
use nom::bytes::complete::{is_not, take_while_m_n};

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
pub fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
  where
      F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
  delimited(
    multispace0,
    inner,
    multispace0,
  )
}

/// A string fragment contains a fragment of a string being parsed: either
/// a non-empty Literal (a series of non-escaped characters), a single
/// parsed escaped character, or a block of escaped whitespace.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringFragment<'a> {
  Literal(&'a str),
  EscapedChar(char),
  EscapedWS,
}

/// Combine parse_literal, parse_escaped_whitespace, and parse_escaped_char
/// into a StringFragment.
fn parse_fragment<'a, E>(input: &'a str) -> IResult<&'a str, StringFragment<'a>, E>
  where
      E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
  alt((
    // The `map` combinator runs a parser, then applies a function to the output
    // of that parser.
    map(parse_literal, StringFragment::Literal),
    map(parse_escaped_char, StringFragment::EscapedChar),
    value(StringFragment::EscapedWS, parse_escaped_whitespace),
  ))(input)
}

/// Parse a unicode sequence, of the form u{XXXX}, where XXXX is 1 to 6
/// hexadecimal numerals. We will combine this later with parse_escaped_char
/// to parse sequences like \u{00AC}.
fn parse_unicode<'a, E>(input: &'a str) -> IResult<&'a str, char, E>
  where
      E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
  // `take_while_m_n` parses between `m` and `n` bytes (inclusive) that match
  // a predicate. `parse_hex` here parses between 1 and 6 hexadecimal numerals.
  let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());

  // `preceeded` takes a prefix parser, and if it succeeds, returns the result
  // of the body parser. In this case, it parses u{XXXX}.
  let parse_delimited_hex = preceded(
    nom::character::complete::char('u'),
    // `delimited` is like `preceded`, but it parses both a prefix and a suffix.
    // It returns the result of the middle parser. In this case, it parses
    // {XXXX}, where XXXX is 1 to 6 hex numerals, and returns XXXX
    delimited(nom::character::complete::char('{'),
              parse_hex,
              nom::character::complete::char('}')),
  );

  // `map_res` takes the result of a parser and applies a function that returns
  // a Result. In this case we take the hex bytes from parse_hex and attempt to
  // convert them to a u32.
  let parse_u32 = map_res(parse_delimited_hex, move |hex| u32::from_str_radix(hex, 16));

  // map_opt is like map_res, but it takes an Option instead of a Result. If
  // the function returns None, map_opt returns an error. In this case, because
  // not all u32 values are valid unicode code points, we have to fallibly
  // convert to char with from_u32.
  map_opt(parse_u32, |value| std::char::from_u32(value))(input)
}

/// Parse an escaped character: \n, \t, \r, \u{00AC}, etc.
fn parse_escaped_char<'a, E>(input: &'a str) -> IResult<&'a str, char, E>
  where
      E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
  preceded(
    nom::character::complete::char('\\'),
    // `alt` tries each parser in sequence, returning the result of
    // the first successful match
    alt((
      parse_unicode,
      // The `value` parser returns a fixed value (the first argument) if its
      // parser (the second argument) succeeds. In these cases, it looks for
      // the marker characters (n, r, t, etc) and returns the matching
      // character (\n, \r, \t, etc).
      value('\n', nom::character::complete::char('n')),
      value('\r', nom::character::complete::char('r')),
      value('\t', nom::character::complete::char('t')),
      value('\u{08}', nom::character::complete::char('b')),
      value('\u{0C}', nom::character::complete::char('f')),
      value('\\', nom::character::complete::char('\\')),
      value('/', nom::character::complete::char('/')),
      value('"', nom::character::complete::char('"')),
    )),
  )(input)
}

/// Parse a backslash, followed by any amount of whitespace. This is used later
/// to discard any escaped whitespace.
fn parse_escaped_whitespace<'a, E: ParseError<&'a str>>(
  input: &'a str,
) -> IResult<&'a str, &'a str, E> {
  preceded(nom::character::complete::char('\\'), multispace1)(input)
}

/// Parse a non-empty block of text that doesn't include \ or "
fn parse_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
  // `is_not` parses a string of 0 or more characters that aren't one of the
  // given characters.
  let not_quote_slash = is_not("\"\\");

  // `verify` runs a parser, then runs a verification function on the output of
  // the parser. The verification function accepts out output only if it
  // returns true. In this case, we want to ensure that the output of is_not
  // is non-empty.
  verify(not_quote_slash, |s: &str| !s.is_empty())(input)
}

/// Parse a string. Use a loop of parse_fragment and push all of the fragments
/// into an output string.
pub fn parse_string<'a, E>(input: &'a str) -> IResult<&'a str, String, E>
  where
      E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
  // fold_many0 is the equivalent of iterator::fold. It runs a parser in a loop,
  // and for each output value, calls a folding function on each output value.
  let build_string = fold_many0(
    // Our parser function– parses a single string fragment
    parse_fragment,
    // Our init value, an empty string
    String::new(),
    // Our folding function. For each fragment, append the fragment to the
    // string.
    |mut string, fragment| {
      match fragment {
        StringFragment::Literal(s) => string.push_str(s),
        StringFragment::EscapedChar(c) => string.push(c),
        StringFragment::EscapedWS => {}
      }
      string
    },
  );

  // Finally, parse the string. Note that, if `build_string` could accept a raw
  // " character, the closing delimiter " would never match. When using
  // `delimited` with a looping parser (like fold_many0), be sure that the
  // loop won't accidentally match your closing delimiter!
  delimited(nom::character::complete::char('"'),
            build_string,
            nom::character::complete::char('"'))(input)
}

pub fn incomplete_parse_error<T>(file_name: &Path, input: &str, remaining: &str) -> ErlResult<T> {
  println!("Parse did not succeed. Remaining input: {}", remaining);
  let span = Span::new(input.len() - remaining.len(), remaining.len());
  Err(ErlParseError(ErrorLocation::SourceFileSpan(file_name.to_path_buf(), span),
                    String::from("Parse did not succeed, input remaining.")))
}
