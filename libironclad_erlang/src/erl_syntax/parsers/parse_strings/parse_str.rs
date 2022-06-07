//! Parse double quoted strings

use crate::erl_syntax::parsers::defs::{ParserInput, ParserResult};
use crate::erl_syntax::parsers::misc::ws_before;
use crate::erl_syntax::parsers::parse_strings::shared;
use crate::erl_syntax::parsers::parse_strings::shared::{parse_escaped_whitespace, StringFragment};
use nom::branch::alt;
use nom::bytes::complete::is_not;
use nom::character::complete::char;
use nom::combinator::{map, value, verify};
use nom::multi::fold_many0;
use nom::sequence::delimited;

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
fn parse_str_fragment<'a>(input: ParserInput<'a>) -> ParserResult<StringFragment<'a>> {
  alt((
    // The `map` combinator runs a parser, then applies a function to the output of that parser.
    map(parse_literal, StringFragment::Literal),
    map(shared::parse_escaped_char, StringFragment::EscapedChar),
    value(StringFragment::EscapedWS, parse_escaped_whitespace),
  ))(input)
}

/// fold_many0 is the equivalent of iterator::fold. It runs a parser in a loop,
/// and for each output value, calls a folding function on each output value.
pub fn build_quoted_str_body(input: ParserInput) -> ParserResult<String> {
  fold_many0(
    // Our parser function– parses a single string fragment
    parse_str_fragment,
    // Our init value, an empty string
    String::new,
    // Our folding function. For each fragment, append the fragment to the string.
    |mut string, fragment| {
      match fragment {
        StringFragment::Literal(s) => string.push_str(s),
        StringFragment::EscapedChar(c) => string.push(c),
        StringFragment::EscapedWS => {}
      }
      string
    },
  )(input)
}

/// Parse a string. Use a loop of parse_fragment and push all of the fragments
/// into an output string.
pub fn parse_doublequot_string<'a>(input: ParserInput) -> ParserResult<String> {
  // Finally, parse the string. Note that, if `build_string` could accept a raw
  // " character, the closing delimiter " would never match. When using
  // `delimited` with a looping parser (like fold_many0), be sure that the
  // loop won't accidentally match your closing delimiter!
  delimited(ws_before(char('\"')), build_quoted_str_body, char('\"'))(input)
}
