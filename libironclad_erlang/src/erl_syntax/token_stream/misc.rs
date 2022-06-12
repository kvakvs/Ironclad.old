//! Tokenizer helpers

use crate::erl_syntax::token_stream::tok_strings::Char;
use crate::erl_syntax::token_stream::tokenizer::{TokInput, TokResult, TokenizerError};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alphanumeric1, anychar, char, multispace1};
use nom::combinator::{eof, map, recognize, verify};
use nom::multi::{many0, many1, many_till};
use nom::sequence::{delimited, pair, preceded};

/// Recognizes newline or end of input
pub(crate) fn newline_or_eof<'a>(input: TokInput<'a>) -> TokResult<TokInput<'a>> {
  recognize(preceded(
    many0(alt((char(' '), char('\t')))),
    alt((tag("\r\n"), tag("\r"), tag("\n"), eof)),
  ))(input)
}

/// Recognizes `% text <newline>` consuming text
pub(crate) fn line_comment<'a>(input: TokInput<'a>) -> TokResult<TokInput<'a>> {
  recognize(preceded(many1(char('%')), many_till(anychar, newline_or_eof)))(input)
}

/// Recognizes 0 or more whitespaces and line comments
fn spaces_or_comments0<'a>(input: TokInput<'a>) -> TokResult<TokInput<'a>> {
  recognize(many0(alt((multispace1, line_comment))))(input)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes leading
/// whitespace, returning the output of `inner`.
pub(crate) fn ws_before<'a, InnerFn: 'a, Out>(
  inner: InnerFn,
) -> impl FnMut(TokInput<'a>) -> TokResult<Out>
where
  InnerFn: Fn(TokInput<'a>) -> TokResult<Out>,
{
  preceded::<TokInput<'a>, TokInput<'a>, Out, TokenizerError, _, InnerFn>(
    spaces_or_comments0,
    inner,
  )
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes leading
/// whitespace, returning the output of `inner`.
pub(crate) fn ws_before_mut<'a, InnerFn: 'a, Out>(
  inner: InnerFn,
) -> impl FnMut(TokInput<'a>) -> TokResult<Out>
where
  InnerFn: FnMut(TokInput<'a>) -> TokResult<Out>,
{
  preceded(spaces_or_comments0, inner)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
pub(crate) fn ws_mut<'a, InnerFn: 'a, Out>(
  inner: InnerFn,
) -> impl FnMut(TokInput<'a>) -> TokResult<Out>
where
  InnerFn: FnMut(TokInput<'a>) -> TokResult<Out>,
{
  delimited(spaces_or_comments0, inner, spaces_or_comments0)
}

#[inline]
fn ident_continuation(input: TokInput) -> TokResult<TokInput> {
  alt((alphanumeric1, recognize(char('_'))))(input)
}

/// Parse an identifier, starting with lowercase and also can be containing numbers and underscoress
pub(crate) fn parse_ident(input: TokInput) -> TokResult<String> {
  map(
    ws_before_mut(recognize(pair(
      verify(anychar, |c: &char| c.is_lowercase()),
      many0(ident_continuation),
    ))),
    |result| result.to_string(),
  )(input)
}

/// Parse an identifier, starting with a letter and also can be containing numbers and underscoress
pub(crate) fn macro_ident(input: TokInput) -> TokResult<String> {
  map(
    ws_before_mut(recognize(pair(
      verify(anychar, |c: &char| c.is_alphabetic() || *c == '_'),
      many0(ident_continuation),
    ))),
    |pi| pi.to_string(),
  )(input)
}

/// Parse an identifier, starting with lowercase and also can be containing numbers and underscoress
pub(crate) fn varname(input: TokInput) -> TokResult<String> {
  map(
    recognize(pair(
      // a variable is a pair of UPPERCASE or _, followed by any alphanum or _
      verify(anychar, |c: &char| c.is_uppercase() || *c == '_'),
      many0(ident_continuation),
    )),
    |result: TokInput| result.to_string(),
  )(input)
}
