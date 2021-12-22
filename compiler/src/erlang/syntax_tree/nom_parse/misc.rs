//! Helper functions for Nom parsing

use nom::{sequence, combinator, multi, branch, character,
          character::complete::{one_of, alphanumeric1, multispace0}, bytes::complete::{tag}};

/// A combinator that takes a parser `inner` and produces a parser that also consumes leading
/// whitespace, returning the output of `inner`.
pub fn ws_before<'a, InnerFn: 'a, Out, ErrType: nom::error::ParseError<&'a str>>(
  inner: InnerFn) -> impl FnMut(&'a str) -> nom::IResult<&'a str, Out, ErrType>
  where InnerFn: Fn(&'a str) -> nom::IResult<&'a str, Out, ErrType>,
{
  sequence::preceded(multispace0, inner)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes leading
/// whitespace, returning the output of `inner`.
pub fn ws_before_mut<'a, InnerFn: 'a, Out, ErrType: nom::error::ParseError<&'a str>>(
  inner: InnerFn) -> impl FnMut(&'a str) -> nom::IResult<&'a str, Out, ErrType>
  where InnerFn: FnMut(&'a str) -> nom::IResult<&'a str, Out, ErrType>,
{
  sequence::preceded(multispace0, inner)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
pub fn ws<'a, InnerFn: 'a, Out, ErrType: nom::error::ParseError<&'a str>>(
  inner: InnerFn) -> impl FnMut(&'a str) -> nom::IResult<&'a str, Out, ErrType>
  where InnerFn: Fn(&'a str) -> nom::IResult<&'a str, Out, ErrType>,
{
  sequence::delimited(multispace0, inner, multispace0)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
pub fn ws_mut<'a, InnerFn: 'a, Out, ErrType: nom::error::ParseError<&'a str>>(
  inner: InnerFn) -> impl FnMut(&'a str) -> nom::IResult<&'a str, Out, ErrType>
  where InnerFn: FnMut(&'a str) -> nom::IResult<&'a str, Out, ErrType>,
{
  sequence::delimited(multispace0, inner, multispace0)
}

/// Parse an identifier, starting with lowercase and also can be containing numbers and underscoress
pub fn parse_ident(input: &str) -> nom::IResult<&str, String> {
  combinator::map(
    combinator::recognize(
      sequence::pair(
        combinator::verify(character::complete::anychar,
                           |c: &char| c.is_alphabetic() && c.is_lowercase()),
        multi::many0(branch::alt((alphanumeric1, tag("_")))),
      )
    ),
    |result: &str| result.to_string(),
  )(input)
}

/// Parse an identifier, starting with lowercase and also can be containing numbers and underscoress
pub fn parse_ident_capitalized(input: &str) -> nom::IResult<&str, String> {
  combinator::map(
    combinator::recognize(
      sequence::pair(
        combinator::verify(character::complete::anychar, |c: &char| c.is_uppercase()),
        multi::many0(branch::alt((alphanumeric1, tag("_")))),
      )
    ),
    |result: &str| result.to_string(),
  )(input)
}

/// Parse an integer without a sign. Signs apply as unary operators. Output is a string.
/// From Nom examples
pub fn parse_int(input: &str) -> nom::IResult<&str, &str> {
  combinator::recognize(
    multi::many1(
      sequence::terminated(one_of("0123456789"),
                           multi::many0(character::complete::char('_')))
    )
  )(input)
}

/// Parse a float with possibly scientific notation. Output is a string.
/// From Nom examples
pub fn parse_float(input: &str) -> nom::IResult<&str, &str> {
  branch::alt((
    // Case one: .42
    combinator::recognize(
      sequence::tuple((
        character::complete::char('.'),
        parse_int,
        combinator::opt(sequence::tuple((
          one_of("eE"),
          combinator::opt(one_of("+-")),
          parse_int
        )))
      ))
    ),
    // Case two: 42e42 and 42.42e42
    combinator::recognize(
      sequence::tuple((
        parse_int,
        combinator::opt(sequence::preceded(
          character::complete::char('.'),
          parse_int,
        )),
        one_of("eE"),
        combinator::opt(one_of("+-")),
        parse_int
      ))
    ),
    // Case three: 42. and 42.42
    combinator::recognize(
      sequence::tuple((
        parse_int,
        character::complete::char('.'),
        combinator::opt(parse_int)
      ))
    )
  ))(input)
}

/// Matches newline \n or \r\n
pub fn newline(input: &str) -> nom::IResult<&str, &str> {
  combinator::recognize(
    branch::alt((combinator::eof, tag("\n"), tag("\r\n")))
  )(input)
}
