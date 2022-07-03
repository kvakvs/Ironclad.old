//! Parse double quoted strings

use crate::erl_syntax::parsers::token_stream::misc::ws_before;
use crate::erl_syntax::parsers::token_stream::tok_input::{TokenizerInput, TokensResult};
use crate::erl_syntax::parsers::token_stream::tok_strings::shared;
use crate::erl_syntax::parsers::token_stream::tok_strings::shared::{
  parse_escaped_whitespace, StringFragment,
};
use crate::typing::erl_integer::ErlInteger;
use nom::branch::alt;
use nom::bytes::complete::is_not;
use nom::character::complete::{alphanumeric1, char, one_of};
use nom::combinator::{map, opt, recognize, value, verify};
use nom::multi::{fold_many0, many0, many1};
use nom::number::complete::recognize_float;
use nom::sequence::{delimited, pair, separated_pair, terminated};

/// Parse a non-empty block of text that doesn't include \ or "
fn parse_doublequot_literal<'a>(input: TokenizerInput<'a>) -> TokensResult<&'a str> {
  // `is_not` parses a string of 0 or more characters that aren't one of the
  // given characters.
  let not_quote_slash = is_not("\"\\");

  // `verify` runs a parser, then runs a verification function on the output of
  // the parser. The verification function accepts out output only if it
  // returns true. In this case, we want to ensure that the output of is_not
  // is non-empty.
  verify(not_quote_slash, |s: &TokenizerInput<'a>| !s.is_empty())(input)
}

/// Combine parse_literal, parse_escaped_whitespace, and parse_escaped_char
/// into a StringFragment.
fn parse_str_fragment<'a>(input: TokenizerInput<'a>) -> TokensResult<StringFragment<'a>> {
  alt((
    // The `map` combinator runs a parser, then applies a function to the output of that parser.
    map(parse_doublequot_literal, StringFragment::Literal),
    map(shared::parse_escaped_char, StringFragment::EscapedChar),
    value(StringFragment::EscapedWS, parse_escaped_whitespace),
  ))(input)
}

/// fold_many0 is the equivalent of iterator::fold. It runs a parser in a loop,
/// and for each output value, calls a folding function on each output value.
pub(crate) fn build_quoted_str_body(input: TokenizerInput) -> TokensResult<String> {
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
pub(crate) fn parse_doublequot_string(input: TokenizerInput) -> TokensResult<String> {
  // Finally, parse the string. Note that, if `build_string` could accept a raw
  // " character, the closing delimiter " would never match. When using
  // `delimited` with a looping parser (like fold_many0), be sure that the
  // loop won't accidentally match your closing delimiter!
  delimited(ws_before(char('\"')), build_quoted_str_body, char('\"'))(input)
}

fn parse_int_unsigned_body(input: TokenizerInput) -> TokensResult<TokenizerInput> {
  recognize(many1(terminated(one_of("0123456789"), many0(char('_')))))(input)
}

/// Parse an any-base integer `0-9, a-z` (check is done when parsing is complete)
fn parse_based_int_unsigned_body(input: TokenizerInput) -> TokensResult<TokenizerInput> {
  recognize(many1(terminated(alphanumeric1, many0(char('_')))))(input)
}

// /// Matches + or -
// fn parse_sign(input: TokenizerInput) -> TokensResult<TokenizerInput> {
//   recognize(alt((char('-'), char('+'))))(input)
// }

/// Parse a decimal integer, without a base prefix and sign
pub fn parse_int_decimal(input: TokenizerInput) -> TokensResult<ErlInteger> {
  map(recognize(pair(opt(char('-')), parse_int_unsigned_body)), |num| {
    ErlInteger::new_from_string(num).unwrap_or_else(|| panic!("Can't parse {} as integer", num))
  })(input)
}

/// Parse a based integer `<BASE> # <NUMBER>` where base is `2..36`
fn parse_based_int(input: TokenizerInput) -> TokensResult<ErlInteger> {
  map(
    pair(
      opt(char('-')),
      separated_pair(parse_int_unsigned_body, char('#'), parse_based_int_unsigned_body),
    ),
    |(sign, (base_str, value_str)): (Option<_>, (&str, &str))| -> ErlInteger {
      let base = base_str.parse::<u32>().unwrap();
      assert!(base >= 2 && base <= 36);
      let value = if sign.is_some() { format!("-{}", value_str) } else { value_str.to_string() };
      ErlInteger::new_from_string_radix(&value, base).unwrap()
    },
  )(input)
}

/// Parse an integer without a sign. Supports based integers with `<RADIX> # <BODY>` and decimals.
/// Sign is not parsed.
pub(crate) fn parse_int_any_base(input: TokenizerInput) -> TokensResult<ErlInteger> {
  alt((parse_based_int, parse_int_decimal))(input)
}

/// Recognize a float in the input and try parse it as `f64`.
pub(crate) fn parse_float(input: TokenizerInput) -> TokensResult<f64> {
  // TODO: handle a parse error or, better, fail the parser?
  let mk_float = |fstr: TokenizerInput| fstr.parse::<f64>().unwrap();
  map(recognize_float, mk_float)(input)
}
