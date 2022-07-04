//! Helper functions for Nom parsing
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::error_report;
use crate::erl_syntax::parsers::lang_construct::LangConstruct;
use crate::erl_syntax::parsers::parser_error::ErlParserError;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::parsers::token_stream::keyword::Keyword;
use crate::erl_syntax::parsers::token_stream::tok_input::TokenizerInput;
use crate::erl_syntax::parsers::token_stream::token::{format_tok_stream, Token};
use crate::erl_syntax::parsers::token_stream::token_type::TokenType;
use crate::erl_syntax::preprocessor::pp_node::pp_type::PreprocessorNodeType;
use crate::typing::erl_integer::ErlInteger;
use ::function_name::named;
use nom::branch::alt;
use nom::combinator::{eof, map, recognize};
use nom::error::{context, convert_error};
use nom::multi::many0;
use nom::sequence::{pair, preceded, tuple};
use nom::Slice;
use std::sync::Arc;

#[inline]
fn void_fn<T>(_in: T) {}

/// Recognizes one token of a given tokentype, the tokentype fields are ignored.
/// *Complete version*: Will return an error if there's not enough input data.
pub fn tok(compare_val: TokenType) -> impl Fn(ParserInput) -> ParserResult<()> {
  move |input: ParserInput| -> ParserResult<()> {
    match input.tokens.iter().next() {
      Some(tok) if tok.content.is_same_type(&compare_val) => Ok((input.slice(1..), ())),
      _other => Err(nom::Err::Error(ErlParserError::token_expected(input, compare_val.clone()))),
    }
  }
}

/// Recognizes one atom of given text value
#[inline]
pub fn tok_atom_of(value: &'static str) -> impl Fn(ParserInput) -> ParserResult<()> {
  move |input: ParserInput| -> ParserResult<()> {
    match input.tokens.iter().next() {
      Some(tok) if tok.is_atom_of(value) => Ok((input.slice(1..), ())),
      _other => Err(nom::Err::Error(ErlParserError::atom_expected(input, value))),
    }
  }
}

/// Matches a `<-> <atom>` pair
#[inline]
pub fn dash_atom<'a>(input: ParserInput<'a>, value: &'static str) -> ParserResult<'a, ()> {
  map(pair(tok_minus, ws_before(tok_atom_of(value))), void_fn)(input)
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

/// Recognizes one keyword token, returns the string.
#[inline]
pub fn tok_any_keyword_or_atom(input: ParserInput) -> ParserResult<String> {
  match input.tokens.iter().next() {
    Some(tok) => match &tok.content {
      TokenType::Keyword(k) => Ok((input.slice(1..), k.to_string())),
      TokenType::Atom(a) => Ok((input.slice(1..), a.clone())),
      _ => Err(nom::Err::Error(ErlParserError::any_keyword_or_atom_expected(input))),
    },
    _ => Err(nom::Err::Error(ErlParserError::any_keyword_or_atom_expected(input))),
  }
}

fn tok_atom_1(input: ParserInput) -> ParserResult<String> {
  match input.tokens.iter().next() {
    Some(Token { content: TokenType::Atom(s), .. }) => Ok((input.slice(1..), s.clone())),
    _other => Err(nom::Err::Error(ErlParserError::any_atom_expected(input))),
  }
}

/// Recognizes one str token, returns the string.
#[inline]
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
  ws_before(alt((tok_var_1, map(tok(TokenType::Underscore), |_| String::from("_")))))(input)
}

#[inline]
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
#[inline]
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

/// Print detailed error with source pointers, and panic.
/// Set `require_empty_tail` to true to panic if the parse did not consume the whole input.
#[named]
pub fn panicking_parser_error_reporter<'a, Out>(
  original_input: &str,
  tokenstream_input: ParserInput,
  res: Result<(ParserInput<'a>, Out), ErlParserError>,
  require_empty_tail: bool,
) -> (ParserInput<'a>, Out) {
  match res {
    Ok((tail, out)) if require_empty_tail => {
      let trim_tail = tail.tokens.iter().filter(|t| !t.is_eol()).count();
      if trim_tail != 0 {
        panic!(
          "Parser: Not all input was consumed: tail=«{}»",
          format_tok_stream(tail.tokens, 50)
        )
      }
      (tail, out)
    }
    Ok((tail, out)) => (tail, out),
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

#[inline]
fn eol_or_eof(input: ParserInput) -> ParserResult<ParserInput> {
  alt((recognize(tok(TokenType::EOL)), eof))(input)
}

/// Recognize the macro end marker `) . \n <EOF>` as we trim the lines before feeding them into the
/// macro parser, there will be an EOF.
#[inline]
pub(crate) fn parenthesis_period_eol_eof(input: ParserInput) -> ParserResult<ParserInput> {
  context(
    "preprocessor directive or a module attribute: ') . <Newline>' expected",
    recognize(tuple((tok_par_close, tok_period, eol_or_eof))),
  )(input)
}

/// Match `. <EOF>` that serves as an end marker for parsing split lines as preprocessor directives.
#[inline]
pub(crate) fn period_eol_eof(input: ParserInput) -> ParserResult<ParserInput> {
  context(
    "preprocessor directive or a module attribute: '. <Newline>' expected",
    recognize(pair(tok_period, eol_or_eof)),
  )(input)
}

// make_tok_match_fn!(tok_minus, Minus);

/// Matches a `-` token with possibly a newline before it
#[inline]
pub(crate) fn tok_minus(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::Minus)), void_fn)(input)
}

/// Matches a `(` token with possibly a newline before it
#[inline]
pub(crate) fn tok_par_open(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::ParOpen)), void_fn)(input)
}

/// Matches a `)` token with possibly a newline before it
#[inline]
pub(crate) fn tok_par_close(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::ParClose)), void_fn)(input)
}

/// Matches a `{` token with possibly a newline before it
#[inline]
pub(crate) fn tok_curly_open(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::CurlyOpen)), void_fn)(input)
}

/// Matches a `}` token with possibly a newline before it
#[inline]
pub(crate) fn tok_curly_close(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::CurlyClose)), void_fn)(input)
}

/// Matches a `<<` token with possibly a newline before it
#[inline]
pub(crate) fn tok_double_angle_open(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::DoubleAngleOpen)), void_fn)(input)
}

/// Matches a `>>` token with possibly a newline before it
#[inline]
pub(crate) fn tok_double_angle_close(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::DoubleAngleClose)), void_fn)(input)
}

/// Matches a `#` token with possibly a newline before it
#[inline]
pub(crate) fn tok_hash(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::Hash)), void_fn)(input)
}

/// Matches a `.` token with possibly a newline before it
#[inline]
pub(crate) fn tok_period(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::Period)), void_fn)(input)
}

/// Matches a `/` token with possibly a newline before it
#[inline]
pub(crate) fn tok_forward_slash(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::ForwardSlash)), void_fn)(input)
}

/// Matches a `,` token with possibly a newline before it
#[inline]
pub(crate) fn tok_comma(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::Comma)), void_fn)(input)
}

/// Matches a `;` token with possibly a newline before it
#[inline]
pub(crate) fn tok_semicolon(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::Semicolon)), void_fn)(input)
}

/// Matches a `:` token with possibly a newline before it
#[inline]
pub(crate) fn tok_colon(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::Colon)), void_fn)(input)
}

/// Matches a `::` token with possibly a newline before it
#[inline]
pub(crate) fn tok_double_colon(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::ColonColon)), void_fn)(input)
}

/// Matches a `|` token with possibly a newline before it
#[inline]
pub(crate) fn tok_vertical_bar(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::VerticalBar)), void_fn)(input)
}

/// Matches a `||` token with possibly a newline before it
#[inline]
pub(crate) fn tok_double_vertical_bar(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::DoubleVerticalBar)), void_fn)(input)
}

/// Matches a `...` token with possibly a newline before it
#[inline]
pub(crate) fn tok_ellipsis(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::Ellipsis)), void_fn)(input)
}

/// Matches a `*` token with possibly a newline before it
#[inline]
pub(crate) fn tok_asterisk(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::Asterisk)), void_fn)(input)
}

/// Matches a `_` token with possibly a newline before it
#[inline]
pub(crate) fn tok_underscore(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::Underscore)), void_fn)(input)
}

/// Matches a `=` token with possibly a newline before it
#[inline]
pub(crate) fn tok_equal(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::EqualSymbol)), void_fn)(input)
}

/// Matches a `..` token with possibly a newline before it
#[inline]
pub(crate) fn tok_double_period(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::PeriodPeriod)), void_fn)(input)
}

/// Matches a `=>` token with possibly a newline before it
#[inline]
pub(crate) fn tok_right_darr(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::RightDoubleArr)), void_fn)(input)
}

/// Matches a `:=` token with possibly a newline before it
#[inline]
pub(crate) fn tok_assign(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::Assign)), void_fn)(input)
}

/// Matches a `->` token with possibly a newline before it
#[inline]
pub(crate) fn tok_right_arrow(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::RightArr)), void_fn)(input)
}

/// Matches a `[` token with possibly a newline before it
#[inline]
pub(crate) fn tok_square_open(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::SquareOpen)), void_fn)(input)
}

/// Matches a `]` token with possibly a newline before it
#[inline]
pub(crate) fn tok_square_close(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::SquareClose)), void_fn)(input)
}

/// Matches a `<-` token with possibly a newline before it
#[inline]
pub(crate) fn tok_left_arrow(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok(TokenType::LeftArr)), void_fn)(input)
}

/// Matches a `when` keyword with possibly a newline before it
#[inline]
pub(crate) fn tok_keyword_when(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok_keyword(Keyword::When)), void_fn)(input)
}

/// Matches a `case` keyword with possibly a newline before it
#[inline]
pub(crate) fn tok_keyword_case(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok_keyword(Keyword::Case)), void_fn)(input)
}

/// Matches a `of` keyword with possibly a newline before it
#[inline]
pub(crate) fn tok_keyword_of(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok_keyword(Keyword::Of)), void_fn)(input)
}

/// Matches a `if` keyword with possibly a newline before it
#[inline]
pub(crate) fn tok_keyword_if(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok_keyword(Keyword::If)), void_fn)(input)
}

/// Matches a `begin` keyword with possibly a newline before it
#[inline]
pub(crate) fn tok_keyword_begin(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok_keyword(Keyword::Begin)), void_fn)(input)
}

/// Matches a `end` keyword with possibly a newline before it
#[inline]
pub(crate) fn tok_keyword_end(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok_keyword(Keyword::End)), void_fn)(input)
}

/// Matches a `fun` keyword with possibly a newline before it
#[inline]
pub(crate) fn tok_keyword_fun(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok_keyword(Keyword::Fun)), void_fn)(input)
}

/// Matches a `catch` keyword with possibly a newline before it
#[inline]
pub(crate) fn tok_keyword_catch(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok_keyword(Keyword::Catch)), void_fn)(input)
}

/// Matches a `try` keyword with possibly a newline before it
#[inline]
pub(crate) fn tok_keyword_try(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok_keyword(Keyword::Try)), void_fn)(input)
}

/// Matches a `else` keyword with possibly a newline before it
#[inline]
pub(crate) fn tok_keyword_else(input: ParserInput) -> ParserResult<()> {
  map(ws_before(tok_keyword(Keyword::Else)), void_fn)(input)
}

/// Produce a compiler error reporting alt variants expected, and none was found.
pub fn alt_failed<'a, T>(
  input: ParserInput<'a>,
  parse_context: &'static str,
  expected_structures: &'a [LangConstruct],
) -> ParserResult<'a, T> {
  let err = ErlParserError::alt(input, parse_context, expected_structures);
  Err(nom::Err::Error(err))
}
