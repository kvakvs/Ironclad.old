//! Helper functions for Nom parsing
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::error_report;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::token_stream::keyword::Keyword;
use crate::erl_syntax::token_stream::tok_input::TokenizerInput;
use crate::erl_syntax::token_stream::token::Token;
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::typing::erl_integer::ErlInteger;
use ::function_name::named;
use nom::combinator::recognize;
use nom::error::{convert_error, ParseError};
use nom::sequence::tuple;
use nom::Slice;
use std::sync::Arc;

/// Recognizes one token of a given tokentype, the tokentype fields are ignored.
/// *Complete version*: Will return an error if there's not enough input data.
pub fn tok(compare_val: TokenType) -> impl Fn(ParserInput) -> ParserResult<()> {
  move |input: ParserInput| -> ParserResult<()> {
    match input
      .tokens
      .iter()
      .next()
      // .map(|next_tok| -> bool { matches!(&next_tok.content, compare_val) })
    {
      Some(tok) if tok.content == compare_val => Ok((input.slice(1..), ())),
      _other => {
        Err(nom::Err::Error(nom::error::VerboseError::from_error_kind(
          input,
          nom::error::ErrorKind::Fail, // TODO: new error TokenExpected
        )))
      }
    }
  }
}

/// Recognizes one atom of given text value
pub fn tok_atom_of(value: &'static str) -> impl Fn(ParserInput) -> ParserResult<()> {
  move |input: ParserInput| -> ParserResult<()> {
    match input.tokens.iter().next() {
      Some(tok) if tok.is_atom_of(value) => Ok((input.slice(1..), ())),
      _other => Err(nom::Err::Error(nom::error::VerboseError::from_error_kind(
        input,
        nom::error::ErrorKind::Fail, // TODO: new error AtomExpected(s)
      ))),
    }
  }
}

/// Recognizes one keyword of given keyword enum value
pub fn tok_keyword(k: Keyword) -> impl Fn(ParserInput) -> ParserResult<()> {
  move |input: ParserInput| -> ParserResult<()> {
    match input.tokens.iter().next() {
      Some(tok) if tok.is_keyword(k) => Ok((input.slice(1..), ())),
      _ => Err(nom::Err::Error(nom::error::VerboseError::from_error_kind(
        input,
        nom::error::ErrorKind::Fail, // TODO: new error KeywordExpected(s)
      ))),
    }
  }
}

/// Recognizes one integer token, returns the integer.
pub fn tok_integer(input: ParserInput) -> ParserResult<ErlInteger> {
  match input.tokens.iter().next() {
    Some(Token { content: TokenType::Integer(i), .. }) => Ok((input.slice(1..), i.clone())),
    _other => Err(nom::Err::Error(nom::error::VerboseError::from_error_kind(
      input,
      nom::error::ErrorKind::Fail, // TODO: new error IntegerExpected
    ))),
  }
}

/// Recognizes one atom token, returns the string.
pub fn tok_atom(input: ParserInput) -> ParserResult<String> {
  match input.tokens.iter().next() {
    Some(Token { content: TokenType::Atom(s), .. }) => Ok((input.slice(1..), s.clone())),
    _other => Err(nom::Err::Error(nom::error::VerboseError::from_error_kind(
      input,
      nom::error::ErrorKind::Fail, // TODO: new error AnyAtomExpected
    ))),
  }
}

/// Recognizes one str token, returns the string.
pub fn tok_string(input: ParserInput) -> ParserResult<Arc<String>> {
  match input.tokens.iter().next() {
    Some(Token { content: TokenType::Str(s), .. }) => Ok((input.slice(1..), s.clone())),
    _other => Err(nom::Err::Error(nom::error::VerboseError::from_error_kind(
      input,
      nom::error::ErrorKind::Fail, // TODO: new error StringExpected
    ))),
  }
}

/// Recognizes one variable name token, returns the string.
pub fn tok_var(input: ParserInput) -> ParserResult<String> {
  match input.tokens.iter().next() {
    Some(Token { content: TokenType::Variable(v), .. }) => Ok((input.slice(1..), v.clone())),
    _other => Err(nom::Err::Error(nom::error::VerboseError::from_error_kind(
      input,
      nom::error::ErrorKind::Fail, // TODO: new error VariableExpected
    ))),
  }
}

/// Recognizes one float token, returns the value.
pub fn tok_float(input: ParserInput) -> ParserResult<f64> {
  match input.tokens.iter().next() {
    Some(Token { content: TokenType::Float(f), .. }) => Ok((input.slice(1..), *f)),
    _other => Err(nom::Err::Error(nom::error::VerboseError::from_error_kind(
      input,
      nom::error::ErrorKind::Fail, // TODO: new error FloatExpected
    ))),
  }
}

// /// Recognizes 0 or more whitespaces and line comments
// fn spaces_or_comments0<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
//   recognize(many0(alt((multispace1, parse_line_comment))))(input)
// }

// /// A combinator that takes a parser `inner` and produces a parser that also consumes leading
// /// whitespace, returning the output of `inner`.
// pub(crate) fn ws_before<'a, InnerFn: 'a, Out>(
//   inner: InnerFn,
// ) -> impl FnMut(ParserInput<'a>) -> ParserResult<Out>
// where
//   InnerFn: Fn(ParserInput<'a>) -> ParserResult<Out>,
// {
//   preceded::<ParserInput<'a>, ParserInput<'a>, Out, ErlParserError, _, InnerFn>(
//     spaces_or_comments0,
//     inner,
//   )
// }

// /// A combinator that takes a parser `inner` and produces a parser that also consumes leading
// /// whitespace, returning the output of `inner`.
// pub(crate) fn ws_before_mut<'a, InnerFn: 'a, Out>(
//   inner: InnerFn,
// ) -> impl FnMut(ParserInput<'a>) -> ParserResult<Out>
// where
//   InnerFn: FnMut(ParserInput<'a>) -> ParserResult<Out>,
// {
//   preceded(spaces_or_comments0, inner)
// }

// /// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
// /// trailing whitespace, returning the output of `inner`.
// #[allow(dead_code)]
// pub(crate) fn ws<'a, InnerFn: 'a, Out>(
//   inner: InnerFn,
// ) -> impl FnMut(ParserInput<'a>) -> ParserResult<Out>
// where
//   InnerFn: Fn(ParserInput<'a>) -> ParserResult<Out>,
// {
//   delimited(spaces_or_comments0, inner, spaces_or_comments0)
// }

// /// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
// /// trailing whitespace, returning the output of `inner`.
// pub(crate) fn ws_mut<'a, InnerFn: 'a, Out>(
//   inner: InnerFn,
// ) -> impl FnMut(ParserInput<'a>) -> ParserResult<Out>
// where
//   InnerFn: FnMut(ParserInput<'a>) -> ParserResult<Out>,
// {
//   delimited(spaces_or_comments0, inner, multispace0)
// }

// /// Parse an identifier, starting with lowercase and also can be containing numbers and underscoress
// pub(crate) fn parse_ident(input: ParserInput) -> ParserResult<String> {
//   map(
//     ws_before_mut(recognize(pair(
//       verify(anychar, |c: &char| c.is_alphabetic() && c.is_lowercase()),
//       many0(alt((alphanumeric1, tag("_".into())))),
//     ))),
//     |result| result.to_string(),
//   )(input)
// }

// /// Parse an identifier, starting with lowercase and also can be containing numbers and underscoress
// pub(crate) fn parse_varname(input: ParserInput) -> ParserResult<String> {
//   map(
//     recognize(pair(
//       // a variable is a pair of UPPERCASE or _, followed by any alphanum or _
//       verify(anychar, |c: &char| c.is_uppercase() || *c == '_'),
//       many0(alt((alphanumeric1, tag("_".into())))),
//     )),
//     |result: ParserInput| result.to_string(),
//   )(input)
// }

// fn parse_int_unsigned_body(input: ParserInput) -> ParserResult<ParserInput> {
//   recognize(many1(terminated(one_of("0123456789"), many0(char('_')))))(input)
// }

// /// Matches + or -
// fn parse_sign(input: ParserInput) -> ParserResult<ParserInput> {
//   recognize(alt((char('-'), char('+'))))(input)
// }

// /// Parse a decimal integer
// fn parse_int_decimal(input: ParserInput) -> ParserResult<ErlInteger> {
//   map(
//     ws_before_mut(recognize(pair(opt(parse_sign), parse_int_unsigned_body))),
//     |num| {
//       ErlInteger::new_from_string(num.as_str())
//         .unwrap_or_else(|| panic!("Can't parse {} as integer", num))
//     },
//   )(input)
// }

// /// Parse an integer without a sign. Signs apply as unary operators. Output is a string.
// /// From Nom examples
// pub(crate) fn parse_int(input: ParserInput) -> ParserResult<ErlInteger> {
//   parse_int_decimal(input)
// }

// /// Parse a float with possibly scientific notation. Output is a string.
// /// From Nom examples
// pub(crate) fn parse_float<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
//   alt((
//     // Case one: .42
//     recognize(tuple((
//       tok(TokenType::Period),
//       tok_integer,
//       opt(tuple((one_of("eE"), opt(one_of("+-")), tok_integer))),
//     ))),
//     // Case two: 42e42 and 42.42e42
//     recognize(tuple((
//       tok_integer,
//       opt(preceded(tok(TokenType::Period), tok_integer)),
//       one_of("eE"),
//       opt(one_of("+-")),
//       tok_integer,
//     ))),
//     // Case three: 42. (disallowed because end of function is also period) and 42.42
//     recognize(tuple((parse_int, char('.'), parse_int))),
//   ))(input)
// }

// /// Recognizes newline or end of input
// pub(crate) fn newline_or_eof<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
//   recognize(preceded(
//     many0(alt((char(' '), char('\t')))),
//     alt((tag("\r\n".into()), tag("\r".into()), tag("\n".into()), eof)),
//   ))(input)
// }
//
// /// Matches an opening parenthesis "(" with 0+ whitespace before
// #[inline]
// pub(crate) fn par_open_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
//   recognize(ws_before(char('(')))(input)
// }
//
// /// Matches a closing parenthesis ")" with 0+ whitespace before
// #[inline]
// pub(crate) fn par_close_tag(input: ParserInput) -> ParserResult<ParserInput> {
//   recognize(ws_before(char(')')))(input)
// }
//
// /// Matches an opening curly bracket "{" with 0+ whitespace before
// #[inline]
// pub(crate) fn curly_open_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
//   recognize(ws_before(char('{')))(input)
// }
//
// /// Matches a closing curly bracket "}" with 0+ whitespace before
// #[inline]
// pub(crate) fn curly_close_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
//   recognize(ws_before(char('}')))(input)
// }
//
// /// Matches an opening square bracket "[" with 0+ whitespace before
// #[inline]
// pub(crate) fn square_open_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
//   recognize(ws_before(char('[')))(input)
// }
//
// /// Matches a closing square bracket "]" with 0+ whitespace before
// #[inline]
// pub(crate) fn square_close_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
//   recognize(ws_before(char(']')))(input)
// }
//
// /// Matches a comma "," with 0+ whitespace before
// #[inline]
// pub(crate) fn comma_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
//   recognize(ws_before(char(',')))(input)
// }
//
// /// Matches a hash symbol `"#"` with 0+ whitespace before
// #[inline]
// pub(crate) fn hash_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
//   recognize(ws_before(char('#')))(input)
// }
//
// /// Matches a period "." with 0+ whitespace before
// #[inline]
// pub(crate) fn period_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
//   recognize(ws_before(char('.')))(input)
// }
//
// /// Recognizes end of a directive or module attribute in `-<attr> ... "." <newline>`
// #[inline]
// pub(crate) fn period_newline_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
//   ws_before_mut(recognize(pair(period_tag, newline_or_eof)))(input)
// }
//
// /// Matches a semicolon ";" with 0+ whitespace before
// #[inline]
// pub(crate) fn semicolon_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
//   recognize(ws_before(char(';')))(input)
// }
//
// /// Matches a double colon "::" with 0+ whitespace before
// #[inline]
// pub(crate) fn colon_colon_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
//   recognize(ws_before(tag("::".into())))(input)
// }
//
// /// Matches a double dot (double period) ".." with 0+ whitespace before
// #[inline]
// pub(crate) fn dot_dot_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
//   recognize(ws_before(tag("..".into())))(input)
// }
//
// /// Matches an equals sign "=" with 0+ whitespace before
// #[inline]
// pub(crate) fn equals_tag<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
//   recognize(ws_before(char('=')))(input)
// }
//
// /// Recognizes `% text <newline>` consuming text
// pub(crate) fn parse_line_comment<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
//   recognize(pair(many1(char('%')), many_till(anychar, newline_or_eof)))(input)
// }

/// Print detailed error with source pointers, and panic
#[named]
pub fn panicking_parser_error_reporter<'a, Out>(
  original_input: &str,
  tokenstream_input: ParserInput,
  res: Result<(ParserInput<'a>, Out), nom::error::VerboseError<ParserInput<'a>>>,
) -> (ParserInput<'a>, Out) {
  match res {
    Ok((tail, out)) => {
      if !tail.is_empty() {
        panic!("Parser: Not all input was consumed: tail=«{:?}»", tail)
      }
      (tail, out)
    }
    Err(e) => {
      println!(
        "Parse error: {}",
        error_report::convert_token_stream_parser_error(original_input, tokenstream_input, e)
      );
      panic!("{}: Parse error", function_name!())
    }
  }
}

/// Print detailed error with source pointers, and panic
#[named]
pub fn panicking_tokenizer_error_reporter<'a, Out>(
  input: TokenizerInput,
  res: Result<(TokenizerInput<'a>, Out), nom::error::VerboseError<TokenizerInput<'a>>>,
) -> Out {
  match res {
    Ok((tail, out)) => {
      let tail_trim_whitespace = tail.trim();

      if !tail_trim_whitespace.is_empty() {
        panic!("Tokenizer: Not all input was consumed: tail=«{}»", tail_trim_whitespace)
      }
      out
    }
    Err(e) => {
      println!("Parse error: {}", convert_error(input, e));
      panic!("{}: Parse error", function_name!())
    }
  }
}

// /// Returns a new parser which recognizes a `<spaces> "-" <spaces> <tag>` and returns it as
// /// a `&str` slice (*recognizes*, i.e. returns with all whitespace included)
// pub(crate) fn match_dash_tag<'a>(
//   t: ParserInput<'a>,
// ) -> impl FnMut(ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
//   recognize(pair(ws_before(char('-')), match_word(t)))
// }

// /// Matches a non-letter, use with `peek` to mark where word ends
// pub(crate) fn word_break<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
//   recognize(not(alphanumeric1))(input)
// }

// /// Matches a tag which is followed by a non-letter (word break)
// pub(crate) fn match_word<'a>(
//   t: ParserInput<'a>,
// ) -> impl FnMut(ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
//   recognize(terminated(ws_before(tag(t)), peek(word_break)))
// }

/// Print function location and trimmed input, for debugging
#[allow(dead_code)]
pub(crate) fn print_input(fn_name: &str, input: ParserInput) {
  let tok_slice: Vec<Token> = input.tokens.iter().take(20).cloned().collect();
  println!("{} input=«{:?}»", fn_name, tok_slice);
}

/// Print function location and trimmed input, for debugging
#[allow(dead_code)]
pub(crate) fn print_tok_input(fn_name: &str, input: TokenizerInput) {
  println!("{} input=«{}»", fn_name, input.chars().take(50).collect::<String>());
}

/// Checks whether `part` slice is a sub-slice of `outer` slice
#[allow(dead_code)]
pub(crate) fn is_part_of(outer: &str, part: &str) -> bool {
  let outer_beg = outer.as_ptr() as usize;
  let outer_end = outer_beg + outer.len();
  let part_beg = part.as_ptr() as usize;
  let part_end = part_beg + part.len();
  part_beg >= outer_beg && part_end <= outer_end
}

#[allow(dead_code)]
pub(crate) fn parenthesis_period_newline(input: ParserInput) -> ParserResult<ParserInput> {
  // TODO: impl newline token for preprocessor/attribute lines
  recognize(tuple((tok(TokenType::ParClose), tok(TokenType::Period))))(input)
}

#[allow(dead_code)]
pub(crate) fn period_newline(input: ParserInput) -> ParserResult<ParserInput> {
  // TODO: impl newline token for preprocessor/attribute lines
  recognize(tok(TokenType::Period))(input)
}
