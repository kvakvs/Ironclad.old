//! Helper functions for Nom parsing
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::error_report;
use crate::erl_syntax::parsers::parser_error::ErlParserError;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::preprocessor::pp_node::pp_type::PreprocessorNodeType;
use crate::erl_syntax::token_stream::keyword::Keyword;
use crate::erl_syntax::token_stream::tok_input::TokenizerInput;
use crate::erl_syntax::token_stream::token::{format_tok_stream, Token};
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::typing::erl_integer::ErlInteger;
use ::function_name::named;
use nom::combinator::{eof, map, recognize};
use nom::error::{context, convert_error};
use nom::multi::many0;
use nom::sequence::{pair, preceded, tuple};
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
      Some(tok) if tok.content.is_same_type(&compare_val) => Ok((input.slice(1..), ())),
      _other => {
        Err(nom::Err::Error(ErlParserError::token_expected(input, compare_val.clone())))
      }
    }
  }
}

/// Recognizes one atom of given text value
pub fn tok_atom_of(value: &'static str) -> impl Fn(ParserInput) -> ParserResult<()> {
  move |input: ParserInput| -> ParserResult<()> {
    match input.tokens.iter().next() {
      Some(tok) if tok.is_atom_of(value) => Ok((input.slice(1..), ())),
      _other => Err(nom::Err::Error(ErlParserError::atom_expected(input, value))),
    }
  }
}

/// Matches a `<-> <atom>` pair
pub fn dash_atom<'a>(input: ParserInput<'a>, value: &'static str) -> ParserResult<'a, ()> {
  map(pair(tok_minus, tok_atom_of(value)), |_| ())(input)
}

/// Recognizes one keyword of given keyword enum value
/// Use `tok_keyword_<name>` to match with possible whitespace
pub fn tok_keyword(k: Keyword) -> impl FnMut(ParserInput) -> ParserResult<()> {
  move |input: ParserInput| -> ParserResult<()> {
    match input.tokens.iter().next() {
      Some(tok) if tok.is_keyword(k) => Ok((input.slice(1..), ())),
      _ => Err(nom::Err::Error(ErlParserError::keyword_expected(input, k))),
    }
  }
}

/// Recognizes one integer token, returns the integer.
#[inline]
pub fn tok_integer(input: ParserInput) -> ParserResult<ErlInteger> {
  ws_before(tok_integer_1)(input)
}

fn tok_integer_1(input: ParserInput) -> ParserResult<ErlInteger> {
  match input.tokens.iter().next() {
    Some(Token { content: TokenType::Integer(i), .. }) => Ok((input.slice(1..), i.clone())),
    _other => Err(nom::Err::Error(ErlParserError::integer_literal_expected(input))),
  }
}

/// Recognizes one atom token, returns the string.
#[inline]
pub fn tok_atom(input: ParserInput) -> ParserResult<String> {
  ws_before(tok_atom_1)(input)
}

fn tok_atom_1(input: ParserInput) -> ParserResult<String> {
  match input.tokens.iter().next() {
    Some(Token { content: TokenType::Atom(s), .. }) => Ok((input.slice(1..), s.clone())),
    _other => Err(nom::Err::Error(ErlParserError::any_atom_expected(input))),
  }
}

/// Recognizes one str token, returns the string.
pub fn tok_string(input: ParserInput) -> ParserResult<Arc<String>> {
  ws_before(tok_string_1)(input)
}

fn tok_string_1(input: ParserInput) -> ParserResult<Arc<String>> {
  match input.tokens.iter().next() {
    Some(Token { content: TokenType::Str(s), .. }) => Ok((input.slice(1..), s.clone())),
    _other => Err(nom::Err::Error(ErlParserError::string_literal_expected(input))),
  }
}

/// Recognizes one variable name token, returns the string.
#[inline]
pub fn tok_var(input: ParserInput) -> ParserResult<String> {
  ws_before(tok_var_1)(input)
}

fn tok_var_1(input: ParserInput) -> ParserResult<String> {
  match input.tokens.iter().next() {
    Some(Token { content: TokenType::Variable(v), .. }) => Ok((input.slice(1..), v.clone())),
    _other => Err(nom::Err::Error(ErlParserError::variable_expected(input))),
  }
}

/// Recognizes one `-module(NAME).` attribute`
pub fn tok_module_name_attr(input: ParserInput) -> ParserResult<String> {
  if let Some(Token { content: TokenType::Preprocessor(pp_node), .. }) = input.tokens.iter().next()
  {
    if let PreprocessorNodeType::ModuleName { name } = &pp_node.content {
      return Ok((input.slice(1..), name.clone()));
    }
  }
  Err(nom::Err::Error(ErlParserError::module_start_attribute_expected(input)))
}

/// Recognizes one float token, returns the value.
#[inline]
pub fn tok_float(input: ParserInput) -> ParserResult<f64> {
  ws_before(tok_float_1)(input)
}

fn tok_float_1(input: ParserInput) -> ParserResult<f64> {
  match input.tokens.iter().next() {
    Some(Token { content: TokenType::Float(f), .. }) => Ok((input.slice(1..), *f)),
    _other => Err(nom::Err::Error(ErlParserError::float_literal_expected(input))),
  }
}

/// Tokens in the token stream, which are considered as whitespace.
#[inline]
fn erl_whitespace<'a>(input: ParserInput<'a>) -> ParserResult<ParserInput<'a>> {
  recognize(tok(TokenType::EOL))(input)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes leading
/// whitespace, returning the output of `inner`.
pub(crate) fn ws_before<'a, InnerFn: 'a, Out>(
  inner: InnerFn,
) -> impl FnMut(ParserInput<'a>) -> ParserResult<Out>
where
  InnerFn: FnMut(ParserInput<'a>) -> ParserResult<Out>,
{
  preceded(many0(erl_whitespace), inner)
}

// /// A combinator that takes a parser `inner` and produces a parser that also consumes leading
// /// whitespace, returning the output of `inner`.
// pub(crate) fn ws_before_mut<'a, InnerFn: 'a, Out>(
//   inner: InnerFn,
// ) -> impl FnMut(ParserInput<'a>) -> ParserResult<Out>
// where
//   InnerFn: FnMut(ParserInput<'a>) -> ParserResult<Out>,
// {
//   preceded(many0(erl_whitespace), inner)
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

/// Print detailed error with source pointers, and panic
#[named]
pub fn panicking_parser_error_reporter<'a, Out>(
  original_input: &str,
  tokenstream_input: ParserInput,
  res: Result<(ParserInput<'a>, Out), ErlParserError>,
) -> (ParserInput<'a>, Out) {
  match res {
    Ok((tail, out)) => {
      let trim_tail = tail.tokens.iter().filter(|t| !t.is_eol()).count();
      if trim_tail != 0 {
        panic!(
          "Parser: Not all input was consumed: tail=«{}»",
          format_tok_stream(tail.tokens, 50)
        )
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

/// Recognize the macro end marker `) . \n <EOF>` as we trim the lines before feeding them into the
/// macro parser, there will be an EOF.
pub(crate) fn parenthesis_period_eol_eof(input: ParserInput) -> ParserResult<ParserInput> {
  context(
    "preprocessor directive or a module attribute: ') . <Newline>' expected",
    recognize(tuple((tok_par_close, tok(TokenType::Period), tok(TokenType::EOL), eof))),
  )(input)
}

/// Match `. <EOF>` that serves as an end marker for parsing split lines as preprocessor directives.
pub(crate) fn period_eol_eof(input: ParserInput) -> ParserResult<ParserInput> {
  context(
    "preprocessor directive or a module attribute: '. <Newline>' expected",
    recognize(tuple((ws_before(tok(TokenType::Period)), tok(TokenType::EOL), eof))),
  )(input)
}

/// Matches a `-` token with possibly a newline before it
#[inline]
pub(crate) fn tok_minus(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::Minus)), |_| ())(input)
}

/// Matches a `(` token with possibly a newline before it
#[inline]
pub(crate) fn tok_par_open(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::ParOpen)), |_| ())(input)
}

/// Matches a `)` token with possibly a newline before it
#[inline]
pub(crate) fn tok_par_close(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::ParClose)), |_| ())(input)
}

/// Matches a `{` token with possibly a newline before it
#[inline]
pub(crate) fn tok_curly_open(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::CurlyOpen)), |_| ())(input)
}

/// Matches a `}` token with possibly a newline before it
#[inline]
pub(crate) fn tok_curly_close(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::CurlyClose)), |_| ())(input)
}

/// Matches a `<<` token with possibly a newline before it
#[inline]
pub(crate) fn tok_double_angle_open(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::DoubleAngleOpen)), |_| ())(input)
}

/// Matches a `>>` token with possibly a newline before it
#[inline]
pub(crate) fn tok_double_angle_close(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::DoubleAngleClose)), |_| ())(input)
}

/// Matches a `#` token with possibly a newline before it
#[inline]
pub(crate) fn tok_hash(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::Hash)), |_| ())(input)
}

/// Matches a `/` token with possibly a newline before it
#[inline]
pub(crate) fn tok_forward_slash(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::ForwardSlash)), |_| ())(input)
}

/// Matches a `,` token with possibly a newline before it
#[inline]
pub(crate) fn tok_comma(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::Comma)), |_| ())(input)
}

/// Matches a `;` token with possibly a newline before it
#[inline]
pub(crate) fn tok_semicolon(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::Semicolon)), |_| ())(input)
}

/// Matches a `:` token with possibly a newline before it
#[inline]
pub(crate) fn tok_colon(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::Colon)), |_| ())(input)
}

/// Matches a `|` token with possibly a newline before it
#[inline]
pub(crate) fn tok_bar(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::VerticalBar)), |_| ())(input)
}

/// Matches a `[` token with possibly a newline before it
#[inline]
pub(crate) fn tok_square_open(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::SquareOpen)), |_| ())(input)
}

/// Matches a `]` token with possibly a newline before it
#[inline]
pub(crate) fn tok_square_close(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::SquareClose)), |_| ())(input)
}

/// Matches a `<-` token with possibly a newline before it
#[inline]
pub(crate) fn tok_left_arrow(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::LeftArr)), |_| ())(input)
}

/// Matches a `when` keyword with possibly a newline before it
#[inline]
pub(crate) fn tok_keyword_when(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok_keyword(Keyword::When)), |_| ())(input)
}

/// Matches a `case` keyword with possibly a newline before it
#[inline]
pub(crate) fn tok_keyword_case(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok_keyword(Keyword::Case)), |_| ())(input)
}

/// Matches a `of` keyword with possibly a newline before it
#[inline]
pub(crate) fn tok_keyword_of(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok_keyword(Keyword::Of)), |_| ())(input)
}

/// Matches a `if` keyword with possibly a newline before it
#[inline]
pub(crate) fn tok_keyword_if(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok_keyword(Keyword::If)), |_| ())(input)
}

/// Matches a `end` keyword with possibly a newline before it
#[inline]
pub(crate) fn tok_keyword_end(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok_keyword(Keyword::End)), |_| ())(input)
}

/// Matches a `fun` keyword with possibly a newline before it
#[inline]
pub(crate) fn tok_keyword_fun(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok_keyword(Keyword::Fun)), |_| ())(input)
}

/// Matches a `catch` keyword with possibly a newline before it
#[inline]
pub(crate) fn tok_keyword_catch(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok_keyword(Keyword::Catch)), |_| ())(input)
}

/// Matches a `try` keyword with possibly a newline before it
#[inline]
pub(crate) fn tok_keyword_try(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok_keyword(Keyword::Try)), |_| ())(input)
}

/// Matches a `else` keyword with possibly a newline before it
#[inline]
pub(crate) fn tok_keyword_else(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok_keyword(Keyword::Else)), |_| ())(input)
}
