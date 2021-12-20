//! Helper functions for Nom parsing

use nom::{sequence, combinator, multi, branch,
          character::complete::{alpha1, alphanumeric1, multispace0}, bytes::complete::{tag}};

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
pub fn ws<'a, InnerFn: 'a, Out, ErrType: nom::error::ParseError<&'a str>>(
  inner: InnerFn) -> impl FnMut(&'a str) -> nom::IResult<&'a str, Out, ErrType>
  where InnerFn: Fn(&'a str) -> nom::IResult<&'a str, Out, ErrType>,
{
  sequence::delimited(
    multispace0,
    inner,
    multispace0,
  )
}

/// Parse an identifier, starting with lowercase and also can be containing numbers and underscoress
pub fn ident(input: &str) -> nom::IResult<&str, String> {
  combinator::map(
    combinator::recognize(
      sequence::pair(
        alpha1,
        multi::many0(branch::alt((alphanumeric1, tag("_")))),
      )
    ),
    |result: &str| result.to_string(),
  )(input)
}
