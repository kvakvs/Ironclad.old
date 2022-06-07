//! Parse double quoted strings

use crate::erl_syntax::parsers::defs::{ParserInput, ParserResult};
use crate::erl_syntax::parsers::parse_atom::build_quoted_atom_body;
use nom::branch::alt;
use nom::bytes::complete::{is_not, take_while_m_n};
use nom::character::complete::{char, multispace1};
use nom::combinator::{map, map_opt, map_res, value, verify};
use nom::sequence::{delimited, preceded};

/// A string fragment contains a fragment of a string being parsed: either
/// a non-empty Literal (a series of non-escaped characters), a single
/// parsed escaped character, or a block of escaped whitespace.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringFragment<'a> {
  Literal(&'a str),
  EscapedChar(char),
  EscapedWS,
}

/// Groups functions for parsing double-quoted strings
pub struct StringParser {}

/// `take_while_m_n` parses between `m` and `n` bytes (inclusive) that match
/// a predicate. `parse_hex` here parses between 1 and 6 hexadecimal numerals.
fn parse_hex(inp0: ParserInput) -> ParserResult<ParserInput> {
  take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit())(inp0)
}

/// `preceded` takes a prefix parser, and if it succeeds, returns the result
/// of the body parser. In this case, it parses u{XXXX}.
fn parse_delimited_hex(input: ParserInput) -> ParserResult<ParserInput> {
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
pub fn parse_u32(input: ParserInput) -> ParserResult<u32> {
  map_res(parse_delimited_hex, move |hex: ParserInput| {
    u32::from_str_radix(hex.as_str(), 16)
  })(input)
}

// parser combinators are constructed from the bottom up:
// first we write parsers for the smallest elements (escaped characters),
// then combine them into larger parsers.
impl StringParser {
  /// Parse a unicode sequence, of the form u{XXXX}, where XXXX is 1 to 6
  /// hexadecimal numerals. We will combine this later with parse_escaped_char
  /// to parse sequences like \u{00AC}.
  fn parse_unicode<'a>(input: ParserInput) -> ParserResult<char> {
    // map_opt is like map_res, but it takes an Option instead of a Result. If
    // the function returns None, map_opt returns an error. In this case, because
    // not all u32 values are valid unicode code points, we have to fallibly
    // convert to char with from_u32.
    map_opt(parse_u32, std::char::from_u32)(input)
  }

  /// Parse an escaped character: \n, \t, \r, \u{00AC}, etc.
  fn parse_escaped_char<'a>(input: ParserInput) -> ParserResult<char> {
    preceded(
      char('\\'),
      // `alt` tries each parser in sequence, returning the result of
      // the first successful match
      alt((
        Self::parse_unicode,
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
      )),
    )(input)
  }

  /// Parse a backslash, followed by any amount of whitespace. This is used later
  /// to discard any escaped whitespace.
  fn parse_escaped_whitespace<'a>(input: ParserInput) -> ParserResult<ParserInput> {
    preceded(char('\\'), multispace1)(input)
  }

  /// Parse a non-empty block of text that doesn't include \ or "
  fn parse_literal<'a>(input: ParserInput<'a>) -> ParserResult<&'a str> {
    // `is_not` parses a string of 0 or more characters that aren't one of the
    // given characters.
    let not_quote_slash = is_not("\"\\");

    // `verify` runs a parser, then runs a verification function on the output of
    // the parser. The verification function accepts out output only if it
    // returns true. In this case, we want to ensure that the output of is_not
    // is non-empty.
    map(
      verify(not_quote_slash, |s: &ParserInput<'a>| !s.is_empty()),
      |out: ParserInput<'a>| out.as_str(),
    )(input)
  }

  /// Combine parse_literal, parse_escaped_whitespace, and parse_escaped_char
  /// into a StringFragment.
  #[allow(dead_code)]
  fn parse_fragment<'a>(input: ParserInput<'a>) -> ParserResult<StringFragment<'a>> {
    alt((
      // The `map` combinator runs a parser, then applies a function to the output
      // of that parser.
      map(Self::parse_literal, StringFragment::Literal),
      map(Self::parse_escaped_char, StringFragment::EscapedChar),
      value(StringFragment::EscapedWS, Self::parse_escaped_whitespace),
    ))(input)
  }

  /// Parse a string. Use a loop of parse_fragment and push all of the fragments
  /// into an output string.
  pub fn parse_string<'a>(input: ParserInput) -> ParserResult<String> {
    // Finally, parse the string. Note that, if `build_string` could accept a raw
    // " character, the closing delimiter " would never match. When using
    // `delimited` with a looping parser (like fold_many0), be sure that the
    // loop won't accidentally match your closing delimiter!
    delimited(char('\"'), build_quoted_atom_body, char('\"'))(input)
  }
}
