//! Complex support code to parse 'delimited' atom strings and atoms in general
//! String parsing code from Nom examples.

use crate::erl_syntax::parsers::defs::ParserInput;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{parse_ident, ws_before_mut};
use crate::erl_syntax::parsers::parse_strings::shared;
use crate::erl_syntax::parsers::parse_strings::shared::{parse_u32, StringFragment};
use nom::branch::alt;
use nom::bytes::complete::is_not;
use nom::character::complete::char;
use nom::combinator::{map, map_opt, value, verify};
use nom::multi::fold_many0;
use nom::sequence::{delimited, preceded};

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
fn parse_escaped_char(input: ParserInput) -> ParserResult<char> {
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
      value('\'', char('\'')),
    )),
  )(input)
}

/// Parse a non-empty block of text that doesn't include \ or "
fn parse_singlequot_literal<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
  // `is_not` parses a string of 0 or more characters that aren't one of the
  // given characters.
  let not_quote_slash = |inp1: ParserInput<'a>| is_not("\'\\")(inp1);

  // `verify` runs a parser, then runs a verification function on the output of
  // the parser. The verification function accepts out output only if it
  // returns true. In this case, we want to ensure that the output of is_not
  // is non-empty.
  verify(not_quote_slash, |inp2: &ParserInput<'a>| !inp2.is_empty())(input)
}

/// Combine parse_literal, parse_escaped_whitespace, and parse_escaped_char
/// into a StringFragment.
fn parse_fragment<'a>(input: ParserInput<'a>) -> ParserResult<StringFragment<'a>> {
  alt((
    // The `map` combinator runs a parser, then applies a function to the output
    // of that parser.
    map(parse_singlequot_literal, |inp1: ParserInput| {
      StringFragment::Literal(inp1.as_str())
    }),
    map(parse_escaped_char, StringFragment::EscapedChar),
    value(StringFragment::EscapedWS, shared::parse_escaped_whitespace),
  ))(input)
}

/// fold_many0 is the equivalent of iterator::fold. It runs a parser in a loop,
/// and for each output value, calls a folding function on each output value.
pub fn build_quoted_atom_body(input: ParserInput) -> ParserResult<String> {
  fold_many0(
    // Our parser function– parses a single string fragment
    parse_fragment,
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
  )(input)
}

/// Parse a string. Use a loop of parse_fragment and push all of the fragments
/// into an output string.
fn parse_quoted_atom<'a>(input: ParserInput) -> ParserResult<String> {
  // Finally, parse the string. Note that, if `build_string` could accept a raw
  // " character, the closing delimiter " would never match. When using
  // `delimited` with a looping parser (like fold_many0), be sure that the
  // loop won't accidentally match your closing delimiter!
  delimited(char('\''), build_quoted_atom_body, char('\''))(input)
}

/// Parse an atom which can either be a naked identifier starting with lowercase, or a single-quoted
/// delitmited string
pub fn parse_atom(input: ParserInput) -> ParserResult<String> {
  ws_before_mut(alt((verify(parse_ident, |s| !is_erl_keyword(s)), parse_quoted_atom)))(input)
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
      | "maybe" // otp 25 new keyword
  )
}
