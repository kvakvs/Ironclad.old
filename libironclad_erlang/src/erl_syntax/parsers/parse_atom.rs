//! Complex support code to parse 'delimited' atom strings and atoms in general
//! String parsing code from Nom examples.

use crate::erl_syntax::parsers::misc::parse_ident;
use crate::erl_syntax::parsers::StringParserResult;
use nom::branch::alt;
use nom::combinator::{map, map_opt, map_res, value, verify};
use nom::multi::fold_many0;
use nom::sequence::{delimited, preceded};
use nom::{
  bytes::streaming::{is_not, take_while_m_n},
  character, error,
};

/// A string fragment contains a fragment of a string being parsed: either
/// a non-empty Literal (a series of non-escaped characters), a single
/// parsed escaped character, or a block of escaped whitespace.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringFragment<'a> {
  Literal(&'a str),
  EscapedChar(char),
  EscapedWS,
}

/// Groups functions for parsing atoms and quoted atoms together
pub struct AtomParser {}

// parser combinators are constructed from the bottom up:
// first we write parsers for the smallest elements (escaped characters),
// then combine them into larger parsers.

impl AtomParser {
  /// Parse a unicode sequence, of the form u{XXXX}, where XXXX is 1 to 6
  /// hexadecimal numerals. We will combine this later with parse_escaped_char
  /// to parse sequences like \u{00AC}.
  fn parse_unicode<'a, E>(input: &'a str) -> nom::IResult<&'a str, char, E>
  where
    E: error::ParseError<&'a str> + error::FromExternalError<&'a str, std::num::ParseIntError>,
  {
    // `take_while_m_n` parses between `m` and `n` bytes (inclusive) that match
    // a predicate. `parse_hex` here parses between 1 and 6 hexadecimal numerals.
    let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());

    // `preceded` takes a prefix parser, and if it succeeds, returns the result
    // of the body parser. In this case, it parses u{XXXX}.
    let parse_delimited_hex = preceded(
      character::streaming::char('u'),
      // `delimited` is like `preceded`, but it parses both a prefix and a suffix.
      // It returns the result of the middle parser. In this case, it parses
      // {XXXX}, where XXXX is 1 to 6 hex numerals, and returns XXXX
      delimited(character::streaming::char('{'), parse_hex, character::streaming::char('}')),
    );

    // `map_res` takes the result of a parser and applies a function that returns
    // a Result. In this case we take the hex bytes from parse_hex and attempt to
    // convert them to a u32.
    let parse_u32 = map_res(parse_delimited_hex, move |hex| u32::from_str_radix(hex, 16));

    // map_opt is like map_res, but it takes an Option instead of a Result. If
    // the function returns None, map_opt returns an error. In this case, because
    // not all u32 values are valid unicode code points, we have to fallibly
    // convert to char with from_u32.
    map_opt(parse_u32, std::char::from_u32)(input)
  }

  /// Parse an escaped character: \n, \t, \r, \u{00AC}, etc.
  fn parse_escaped_char<'a, E>(input: &'a str) -> nom::IResult<&'a str, char, E>
  where
    E: error::ParseError<&'a str> + error::FromExternalError<&'a str, std::num::ParseIntError>,
  {
    preceded(
      character::streaming::char('\\'),
      // `alt` tries each parser in sequence, returning the result of
      // the first successful match
      alt((
        Self::parse_unicode,
        // The `value` parser returns a fixed value (the first argument) if its
        // parser (the second argument) succeeds. In these cases, it looks for
        // the marker characters (n, r, t, etc) and returns the matching
        // character (\n, \r, \t, etc).
        value('\n', character::streaming::char('n')),
        value('\r', character::streaming::char('r')),
        value('\t', character::streaming::char('t')),
        value('\u{08}', character::streaming::char('b')),
        value('\u{0C}', character::streaming::char('f')),
        value('\\', character::streaming::char('\\')),
        value('/', character::streaming::char('/')),
        value('\'', character::streaming::char('\'')),
      )),
    )(input)
  }

  /// Parse a backslash, followed by any amount of whitespace. This is used later
  /// to discard any escaped whitespace.
  fn parse_escaped_whitespace<'a, E: error::ParseError<&'a str>>(
    input: &'a str,
  ) -> nom::IResult<&'a str, &'a str, E> {
    preceded(character::streaming::char('\\'), character::streaming::multispace1)(input)
  }

  /// Parse a non-empty block of text that doesn't include \ or "
  fn parse_literal<'a, E: error::ParseError<&'a str>>(
    input: &'a str,
  ) -> nom::IResult<&'a str, &'a str, E> {
    // `is_not` parses a string of 0 or more characters that aren't one of the
    // given characters.
    let not_quote_slash = is_not("\'\\");

    // `verify` runs a parser, then runs a verification function on the output of
    // the parser. The verification function accepts out output only if it
    // returns true. In this case, we want to ensure that the output of is_not
    // is non-empty.
    verify(not_quote_slash, |s: &str| !s.is_empty())(input)
  }

  /// Combine parse_literal, parse_escaped_whitespace, and parse_escaped_char
  /// into a StringFragment.
  fn parse_fragment<'a, E>(input: &'a str) -> nom::IResult<&'a str, StringFragment<'a>, E>
  where
    E: error::ParseError<&'a str> + error::FromExternalError<&'a str, std::num::ParseIntError>,
  {
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
  fn parse_quoted_atom<'a, E>(input: &'a str) -> nom::IResult<&'a str, String, E>
  where
    E: error::ParseError<&'a str> + error::FromExternalError<&'a str, std::num::ParseIntError>,
  {
    // fold_many0 is the equivalent of iterator::fold. It runs a parser in a loop,
    // and for each output value, calls a folding function on each output value.
    let build_quoted_atom_body = fold_many0(
      // Our parser function– parses a single string fragment
      Self::parse_fragment,
      // Our init value, an empty string
      String::new,
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
    delimited(
      character::streaming::char('\''),
      build_quoted_atom_body,
      character::streaming::char('\''),
    )(input)
  }

  /// Parse an atom which can either be a naked identifier starting with lowercase, or a single-quoted
  /// delitmited string
  pub fn parse_atom(input: &str) -> StringParserResult {
    alt((verify(parse_ident, |s| !Self::is_erl_keyword(s)), Self::parse_quoted_atom))(input)
  }

  fn is_erl_keyword(s: &str) -> bool {
    matches!(
      s,
      "after"
        | "and"
        | "andalso"
        | "band"
        | "begin"
        | "bnot"
        | "bor"
        | "bsl"
        | "bsr"
        | "bxor"
        | "case"
        | "catch"
        | "cond"
        | "div"
        | "end"
        | "fun"
        | "if"
        | "let"
        | "not"
        | "of"
        | "or"
        | "orelse"
        | "receive"
        | "rem"
        | "try"
        | "when"
        | "xor"
    )
  }
}
