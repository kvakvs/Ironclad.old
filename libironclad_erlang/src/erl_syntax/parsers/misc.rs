//! Helper functions for Nom parsing
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::defs::{ErlParserError, ParserInput};
use crate::typing::erl_integer::ErlInteger;
use ::function_name::named;
use nom::branch::alt;
use nom::character::complete::{anychar, multispace0};
use nom::combinator::{eof, map, not, opt, peek, recognize, verify};
use nom::error::convert_error;
use nom::multi::{many0, many1, many_till};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::{
  bytes::complete::tag,
  character::complete::{alphanumeric1, char, multispace1, one_of},
};

/// Recognizes 0 or more whitespaces and line comments
fn spaces_or_comments0<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
  recognize(many0(alt((multispace1, parse_line_comment))))(input)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes leading
/// whitespace, returning the output of `inner`.
pub fn ws_before<'a, InnerFn: 'a, Out>(
  inner: InnerFn,
) -> impl FnMut(ParserInput<'a>) -> ParserResult<Out>
where
  InnerFn: Fn(ParserInput<'a>) -> ParserResult<Out>,
{
  preceded::<ParserInput<'a>, ParserInput<'a>, Out, ErlParserError, _, InnerFn>(
    spaces_or_comments0,
    inner,
  )
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes leading
/// whitespace, returning the output of `inner`.
pub fn ws_before_mut<'a, InnerFn: 'a, Out>(
  inner: InnerFn,
) -> impl FnMut(ParserInput<'a>) -> ParserResult<Out>
where
  InnerFn: FnMut(ParserInput<'a>) -> ParserResult<Out>,
{
  preceded(spaces_or_comments0, inner)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
pub fn ws<'a, InnerFn: 'a, Out>(inner: InnerFn) -> impl FnMut(ParserInput<'a>) -> ParserResult<Out>
where
  InnerFn: Fn(ParserInput<'a>) -> ParserResult<Out>,
{
  delimited(spaces_or_comments0, inner, spaces_or_comments0)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
pub fn ws_mut<'a, InnerFn: 'a, Out>(
  inner: InnerFn,
) -> impl FnMut(ParserInput<'a>) -> ParserResult<Out>
where
  InnerFn: FnMut(ParserInput<'a>) -> ParserResult<Out>,
{
  delimited(spaces_or_comments0, inner, multispace0)
}

/// Parse an identifier, starting with lowercase and also can be containing numbers and underscoress
pub fn parse_ident(input: ParserInput) -> ParserResult<String> {
  map(
    ws_before_mut(recognize(pair(
      verify(anychar, |c: &char| c.is_alphabetic() && c.is_lowercase()),
      many0(alt((alphanumeric1, tag("_".into())))),
    ))),
    |result| result.to_string(),
  )(input)
}

/// Parse an identifier, starting with lowercase and also can be containing numbers and underscoress
pub fn parse_varname(input: ParserInput) -> ParserResult<String> {
  map(
    recognize(pair(
      // a variable is a pair of UPPERCASE or _, followed by any alphanum or _
      verify(anychar, |c: &char| c.is_uppercase() || *c == '_'),
      many0(alt((alphanumeric1, tag("_".into())))),
    )),
    |result: ParserInput| result.to_string(),
  )(input)
}

fn parse_int_unsigned_body(input: ParserInput) -> ParserResult<ParserInput> {
  recognize(many1(terminated(one_of("0123456789"), many0(char('_')))))(input)
}

/// Matches + or -
fn parse_sign(input: ParserInput) -> ParserResult<ParserInput> {
  recognize(alt((char('-'), char('+'))))(input)
}

/// Parse a decimal integer
fn parse_int_decimal(input: ParserInput) -> ParserResult<ErlInteger> {
  map(
    ws_before_mut(recognize(pair(opt(parse_sign), parse_int_unsigned_body))),
    |num| {
      ErlInteger::new_from_string(num.clone())
        .unwrap_or_else(|| panic!("Can't parse {} as integer", num))
    },
  )(input)
}

/// Parse an integer without a sign. Signs apply as unary operators. Output is a string.
/// From Nom examples
pub fn parse_int(input: ParserInput) -> ParserResult<ErlInteger> {
  parse_int_decimal(input)
}

/// Parse a float with possibly scientific notation. Output is a string.
/// From Nom examples
pub fn parse_float<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
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

/// Recognizes newline or end of input
pub fn newline_or_eof<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
  recognize(preceded(
    many0(alt((char(' '), char('\t')))),
    alt((tag("\r\n".into()), tag("\r".into()), tag("\n".into()), eof)),
  ))(input)
}

/// Matches an opening parenthesis "(" with 0+ whitespace before
#[inline]
pub fn par_open_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
  recognize(ws_before(char('(')))(input)
}

/// Matches a closing parenthesis ")" with 0+ whitespace before
#[inline]
pub fn par_close_tag(input: ParserInput) -> ParserResult<ParserInput> {
  recognize(ws_before(char(')')))(input)
}

/// Matches an opening curly bracket "{" with 0+ whitespace before
#[inline]
pub fn curly_open_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
  recognize(ws_before(char('{')))(input)
}

/// Matches a closing curly bracket "}" with 0+ whitespace before
#[inline]
pub fn curly_close_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
  recognize(ws_before(char('}')))(input)
}

/// Matches an opening square bracket "[" with 0+ whitespace before
#[inline]
pub fn square_open_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
  recognize(ws_before(char('[')))(input)
}

/// Matches a closing square bracket "]" with 0+ whitespace before
#[inline]
pub fn square_close_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
  recognize(ws_before(char(']')))(input)
}

/// Matches a comma "," with 0+ whitespace before
#[inline]
pub fn comma_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
  recognize(ws_before(char(',')))(input)
}

/// Matches a hash symbol `"#"` with 0+ whitespace before
#[inline]
pub fn hash_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
  recognize(ws_before(char('#')))(input)
}

/// Matches a period "." with 0+ whitespace before
#[inline]
pub fn period_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
  recognize(ws_before(char('.')))(input)
}

/// Recognizes end of a directive or module attribute in `-<attr> ... "." <newline>`
#[inline]
pub fn period_newline_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
  ws_before_mut(recognize(pair(period_tag, newline_or_eof)))(input)
}

/// Matches a semicolon ";" with 0+ whitespace before
#[inline]
pub fn semicolon_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
  recognize(ws_before(char(';')))(input)
}

/// Matches a double colon "::" with 0+ whitespace before
#[inline]
pub fn colon_colon_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
  recognize(ws_before(tag("::".into())))(input)
}

/// Matches a double dot (double period) ".." with 0+ whitespace before
#[inline]
pub fn dot_dot_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
  recognize(ws_before(tag("..".into())))(input)
}

/// Matches an equals sign "=" with 0+ whitespace before
#[inline]
pub fn equals_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
  recognize(ws_before(char('=')))(input)
}

/// Recognizes `% text <newline>` consuming text
pub fn parse_line_comment<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
  recognize(pair(many1(char('%')), many_till(anychar, newline_or_eof)))(input)
}

/// Print detailed error with source pointers, and panic
#[named]
pub fn panicking_parser_error_reporter<'a, Out>(
  input: ParserInput,
  res: Result<(ParserInput<'a>, Out), nom::error::VerboseError<ParserInput<'a>>>,
) -> (ParserInput<'a>, Out) {
  match res {
    Ok((tail, out)) => {
      let tail_trim_whitespace = tail.trim();

      if !tail_trim_whitespace.is_empty() {
        panic!("Not all input was consumed: tail=«{}»", tail_trim_whitespace)
      }
      (tail, out)
    }
    Err(e) => {
      println!("Parse error: {}", convert_error(input, e));
      panic!("{}: Parse error", function_name!())
    }
  }
}

/// Returns a new parser which recognizes a `<spaces> "-" <spaces> <tag>` and returns it as
/// a `&str` slice (*recognizes*, i.e. returns with all whitespace included)
pub fn match_dash_tag<'a>(
  t: ParserInput<'a>,
) -> impl FnMut(ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
  recognize(pair(ws_before(char('-')), match_word(t)))
}

/// Matches a non-letter, use with `peek` to mark where word ends
pub fn word_break<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
  recognize(not(alphanumeric1))(input)
}

/// Matches a tag which is followed by a non-letter (word break)
pub fn match_word<'a>(
  t: ParserInput<'a>,
) -> impl FnMut(ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
  recognize(terminated(ws_before(tag(t)), peek(word_break)))
}

/// Print function location and trimmed input, for debugging
pub fn print_input(fn_name: &str, input: ParserInput) {
  println!("{} input=«{}»", fn_name, input.chars().take(50).collect::<String>());
}

/// Checks whether `part` slice is a sub-slice of `outer` slice
pub fn is_part_of(outer: &str, part: &str) -> bool {
  let outer_beg = outer.as_ptr() as usize;
  let outer_end = outer_beg + outer.len();
  let part_beg = part.as_ptr() as usize;
  let part_end = part_beg + part.len();
  part_beg >= outer_beg && part_end <= outer_end
}
