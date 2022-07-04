//! Token helpers

use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{tok_keyword, ws_before};
use crate::erl_syntax::parsers::parser_error::ErlParserError;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::parsers::token_stream::keyword::Keyword;
use crate::erl_syntax::parsers::token_stream::token_type::TokenType;
use nom::combinator::map;
use nom::Slice;

#[inline]
fn void_fn<T>(_in: T) {}

/// Recognizes one token of a given tokentype, the tokentype fields are ignored.
/// *Complete version*: Will return an error if there's not enough input data.
fn tok(compare_val: TokenType) -> impl Fn(ParserInput) -> ParserResult<()> {
  move |input: ParserInput| -> ParserResult<()> {
    match input.tokens.iter().next() {
      Some(tok) if tok.content.is_same_type(&compare_val) => Ok((input.slice(1..), ())),
      _other => Err(nom::Err::Error(ErlParserError::token_expected(input, compare_val.clone()))),
    }
  }
}

macro_rules! make_tok_fn {
  // Arguments are module name and function name of function to test bench
  ($tok_name:ident, TokenType :: $tok_type:ident) => {
    // The macro will expand into the contents of this block.
    paste::item! {
        #[inline]
        pub fn [< $tok_name >] (input: ParserInput) -> ParserResult<()> {
            map(ws_before(tok(TokenType :: $tok_type)), void_fn)(input)
        }
    }
  };
}

// - -- + ++ !
make_tok_fn!(tok_minus, TokenType::Minus);
make_tok_fn!(tok_minus_minus, TokenType::ListSubtract);
make_tok_fn!(tok_plus, TokenType::Plus);
make_tok_fn!(tok_plus_plus, TokenType::ListAppend);
make_tok_fn!(tok_send, TokenType::Send);

// ( )
make_tok_fn!(tok_par_open, TokenType::ParOpen);
make_tok_fn!(tok_par_close, TokenType::ParClose);

// { }
make_tok_fn!(tok_curly_open, TokenType::CurlyOpen);
make_tok_fn!(tok_curly_close, TokenType::CurlyClose);

// << >>
make_tok_fn!(tok_double_angle_open, TokenType::DoubleAngleOpen);
make_tok_fn!(tok_double_angle_close, TokenType::DoubleAngleClose);

// # . / , ;
make_tok_fn!(tok_hash, TokenType::Hash);
make_tok_fn!(tok_period, TokenType::Period);
make_tok_fn!(tok_forward_slash, TokenType::ForwardSlash);
make_tok_fn!(tok_comma, TokenType::Comma);
make_tok_fn!(tok_semicolon, TokenType::Semicolon);

// : :: | || ...
make_tok_fn!(tok_colon, TokenType::Colon);
make_tok_fn!(tok_double_colon, TokenType::ColonColon);
make_tok_fn!(tok_vertical_bar, TokenType::VerticalBar);
make_tok_fn!(tok_double_vertical_bar, TokenType::DoubleVerticalBar);
make_tok_fn!(tok_ellipsis, TokenType::Ellipsis);

// * _ = == /=
make_tok_fn!(tok_asterisk, TokenType::Asterisk);
make_tok_fn!(tok_underscore, TokenType::Underscore);
make_tok_fn!(tok_equal_symbol, TokenType::EqualSymbol);
make_tok_fn!(tok_equal_equal, TokenType::EqualEqual);
make_tok_fn!(tok_not_equal, TokenType::NotEq);

// < > =< >=
make_tok_fn!(tok_angle_open, TokenType::AngleOpen);
make_tok_fn!(tok_angle_close, TokenType::AngleClose);
make_tok_fn!(tok_less_eq, TokenType::LessThanEq);
make_tok_fn!(tok_greater_eq, TokenType::GreaterEq);

// =/= =:= .. => :=
make_tok_fn!(tok_hard_not_equal, TokenType::HardNotEq);
make_tok_fn!(tok_hard_equal, TokenType::HardEq);
make_tok_fn!(tok_double_period, TokenType::PeriodPeriod);
make_tok_fn!(tok_right_darr, TokenType::RightDoubleArr);
make_tok_fn!(tok_assign, TokenType::Assign);

// -> <-- [ ] \n
make_tok_fn!(tok_left_arrow, TokenType::LeftArr);
make_tok_fn!(tok_right_arrow, TokenType::RightArr);
make_tok_fn!(tok_square_open, TokenType::SquareOpen);
make_tok_fn!(tok_square_close, TokenType::SquareClose);

#[inline]
pub fn tok_eol(input: ParserInput) -> ParserResult<()> {
  map(tok(TokenType::EOL), void_fn)(input)
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
