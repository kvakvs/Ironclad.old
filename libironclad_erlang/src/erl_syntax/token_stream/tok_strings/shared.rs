//! Shared code for string and atom parsers

use crate::erl_syntax::token_stream::tok_input::{TokenizerInput, TokensResult};
use crate::erl_syntax::token_stream::tok_strings::Char;
use nom::branch::alt;
use nom::bytes::complete::take_while_m_n;
use nom::character::complete::{char, multispace1};
use nom::combinator::{map_opt, map_res, value};
use nom::sequence::{delimited, preceded};

/// A string fragment contains a fragment of a string being parsed: either
/// a non-empty Literal (a series of non-escaped characters), a single
/// parsed escaped character, or a block of escaped whitespace.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringFragment<'a> {
  /// Contains a fragment of string text without escapes/breaks
  Literal(&'a str),
  /// Contains a `\\something`
  EscapedChar(char),
  /// Contains a whitespace which does not count for the string (linebreak tab stuff?)
  EscapedWS,
}

/// Parse a backslash, followed by any amount of whitespace. This is used later
/// to discard any escaped whitespace.
pub(crate) fn parse_escaped_whitespace<'a>(
  input: TokenizerInput<'a>,
) -> TokensResult<TokenizerInput<'a>> {
  preceded(char('\\'), multispace1)(input)
}

/// `take_while_m_n` parses between `m` and `n` bytes (inclusive) that match
/// a predicate. `parse_hex` here parses between 1 and 6 hexadecimal numerals.
fn parse_hex(inp0: TokenizerInput) -> TokensResult<TokenizerInput> {
  take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit())(inp0)
}

/// `preceded` takes a prefix parser, and if it succeeds, returns the result
/// of the body parser. In this case, it parses u{XXXX}.
fn parse_delimited_hex(input: TokenizerInput) -> TokensResult<TokenizerInput> {
  preceded(
    char('u'),
    // `delimited` is like `preceded`, but it parses both a prefix and a suffix.
    // It returns the result of the middle parser. In this case, it parses
    // {XXXX}, where XXXX is 1 to 6 hex numerals, and returns XXXX
    delimited(char('{'), parse_hex, char('}')),
  )(input)
}

/// `map_res` takes the result of a parser and applies a function that returns
/// a Result. In this case we take the hex bytes from parse_hex and attempt to
/// convert them to a u32.
// TODO: Bignums!
pub(crate) fn parse_u32(input: TokenizerInput) -> TokensResult<u32> {
  map_res(parse_delimited_hex, move |hex: TokenizerInput| u32::from_str_radix(hex, 16))(input)
}

// parser combinators are constructed from the bottom up:
// first we write parsers for the smallest elements (escaped characters),
// then combine them into larger parsers.
/// Parse a unicode sequence, of the form u{XXXX}, where XXXX is 1 to 6
/// hexadecimal numerals. We will combine this later with parse_escaped_char
/// to parse sequences like \u{00AC}.
fn parse_unicode(input: TokenizerInput) -> TokensResult<Char> {
  // map_opt is like map_res, but it takes an Option instead of a Result. If
  // the function returns None, map_opt returns an error. In this case, because
  // not all u32 values are valid unicode code points, we have to fallibly
  // convert to char with from_u32.
  map_opt(parse_u32, std::char::from_u32)(input)
}

/// Parse an escaped character: \n, \t, \r, \u{00AC}, etc.
pub(crate) fn parse_escaped_char(input: TokenizerInput) -> TokensResult<Char> {
  preceded(
    char('\\'),
    // `alt` tries each parser in sequence, returning the result of
    // the first successful match
    alt((
      parse_unicode,
      // The `value` parser returns a fixed value (the first argument) if its
      // parser (the second argument) succeeds. In these cases, it looks for
      // the marker characters (n, r, t, etc) and returns the matching
      // character (\n, \r, \t, etc).
      value('\n', char('n')),
      value('\r', char('r')),
      value('\t', char('t')),
      value('\u{08}', char('b')),
      value('\u{0C}', char('f')),
      value('\\', char('\\')),
      value('/', char('/')),
      value('"', char('"')),
      value('\'', char('\'')),
    )),
  )(input)
}
