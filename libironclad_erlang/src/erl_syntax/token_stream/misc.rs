//! Tokenizer helpers

use crate::erl_syntax::token_stream::tok_input::{TokenizerError, TokenizerInput, TokensResult};
use crate::erl_syntax::token_stream::tok_strings::Char;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alphanumeric1, anychar, char, multispace1};
use nom::combinator::{eof, map, recognize, verify};
use nom::multi::{many0, many1, many_till};
use nom::sequence::{delimited, pair, preceded};

/// Recognizes newline or end of input
pub(crate) fn newline_or_eof<'a>(input: TokenizerInput<'a>) -> TokensResult<TokenizerInput<'a>> {
  recognize(preceded(
    many0(alt((char(' '), char('\t')))),
    alt((tag("\r\n"), tag("\r"), tag("\n"), eof)),
  ))(input)
}

/// Recognizes `% text <newline>` consuming text
pub(crate) fn line_comment<'a>(input: TokenizerInput<'a>) -> TokensResult<TokenizerInput<'a>> {
  recognize(preceded(many1(char('%')), many_till(anychar, newline_or_eof)))(input)
}

fn space_only<'a>(input: TokenizerInput<'a>) -> TokensResult<TokenizerInput<'a>> {
  recognize(many1(alt((char(' '), char('\t')))))(input)
}

/// Recognizes 0 or more whitespaces and line comments
fn spaces_or_comments0<'a>(input: TokenizerInput<'a>) -> TokensResult<TokenizerInput<'a>> {
  recognize(many0(alt((
    //multispace1,
    space_only,
    line_comment,
  ))))(input)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes leading
/// whitespace, returning the output of `inner`.
pub(crate) fn ws_before<'a, InnerFn: 'a, Out>(
  inner: InnerFn,
) -> impl FnMut(TokenizerInput<'a>) -> TokensResult<Out>
where
  InnerFn: Fn(TokenizerInput<'a>) -> TokensResult<Out>,
{
  preceded::<TokenizerInput<'a>, TokenizerInput<'a>, Out, TokenizerError, _, InnerFn>(
    spaces_or_comments0,
    inner,
  )
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes leading
/// whitespace, returning the output of `inner`.
pub(crate) fn ws_before_mut<'a, InnerFn: 'a, Out>(
  inner: InnerFn,
) -> impl FnMut(TokenizerInput<'a>) -> TokensResult<Out>
where
  InnerFn: FnMut(TokenizerInput<'a>) -> TokensResult<Out>,
{
  preceded(spaces_or_comments0, inner)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
pub(crate) fn ws_mut<'a, InnerFn: 'a, Out>(
  inner: InnerFn,
) -> impl FnMut(TokenizerInput<'a>) -> TokensResult<Out>
where
  InnerFn: FnMut(TokenizerInput<'a>) -> TokensResult<Out>,
{
  delimited(spaces_or_comments0, inner, spaces_or_comments0)
}

#[inline]
fn ident_continuation(input: TokenizerInput) -> TokensResult<TokenizerInput> {
  alt((alphanumeric1, recognize(char('_'))))(input)
}

/// Parse an identifier, starting with lowercase and also can be containing numbers and underscoress
pub(crate) fn parse_ident(input: TokenizerInput) -> TokensResult<String> {
  map(
    ws_before_mut(recognize(pair(
      verify(anychar, |c: &char| c.is_lowercase()),
      many0(ident_continuation),
    ))),
    |result| result.to_string(),
  )(input)
}

/// Parse an identifier, starting with a letter and also can be containing numbers and underscoress
pub(crate) fn macro_ident(input: TokenizerInput) -> TokensResult<String> {
  map(
    ws_before_mut(recognize(pair(
      verify(anychar, |c: &char| c.is_alphabetic() || *c == '_'),
      many0(ident_continuation),
    ))),
    |pi| pi.to_string(),
  )(input)
}

/// Parse an identifier, starting with lowercase and also can be containing numbers and underscoress
pub(crate) fn varname(input: TokenizerInput) -> TokensResult<String> {
  map(
    recognize(pair(
      // a variable is a pair of UPPERCASE or _, followed by any alphanum or _
      verify(anychar, |c: &char| c.is_uppercase() || *c == '_'),
      many0(ident_continuation),
    )),
    |result: TokenizerInput| result.to_string(),
  )(input)
}

pub fn bigcapacity_many0<I, O, E, F>(mut f: F) -> impl FnMut(I) -> nom::IResult<I, Vec<O>, E>
where
  I: Clone + nom::InputLength,
  F: nom::Parser<I, O, E>,
  E: nom::error::ParseError<I>,
{
  move |mut i: I| {
    let mut acc = Vec::with_capacity(5000);
    loop {
      let len = i.input_len();
      match f.parse(i.clone()) {
        Err(nom::Err::Error(_)) => return Ok((i, acc)),
        Err(e) => return Err(e),
        Ok((i1, o)) => {
          // infinite loop check: the parser must always consume
          if i1.input_len() == len {
            return Err(nom::Err::Error(E::from_error_kind(i, nom::error::ErrorKind::Many0)));
          }

          i = i1;
          acc.push(o);
        }
      }
    }
  }
}
