//! Token helpers

use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::ws_before;
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
        pub(crate) fn [< $tok_name >] (input: ParserInput) -> ParserResult<()> {
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

/// Match an end of line token
#[inline]
pub fn tok_eol(input: ParserInput) -> ParserResult<()> {
  map(tok(TokenType::EOL), void_fn)(input)
}

/// Recognizes one keyword of given keyword enum value
/// Use `tok_keyword_<name>` to match with possible whitespace
fn tok_keyword(k: Keyword) -> impl FnMut(ParserInput) -> ParserResult<()> {
  move |input: ParserInput| -> ParserResult<()> {
    match input.tokens.iter().next() {
      Some(tok) if tok.is_keyword(k) => Ok((input.slice(1..), ())),
      _ => Err(nom::Err::Error(ErlParserError::keyword_expected(input, k))),
    }
  }
}

macro_rules! make_keyword_fn {
  // Arguments are module name and function name of function to test bench
  ($tok_name:ident, Keyword :: $kw_type:ident) => {
    // The macro will expand into the contents of this block.
    paste::item! {
        #[inline]
        pub(crate) fn [< $tok_name >](input: ParserInput) -> ParserResult<()> {
          map(ws_before(tok_keyword(Keyword :: $kw_type)), void_fn)(input)
        }
    }
  };
}

make_keyword_fn!(keyword_and, Keyword::And);
make_keyword_fn!(keyword_andalso, Keyword::AndAlso);
make_keyword_fn!(keyword_band, Keyword::BinaryAnd);
make_keyword_fn!(keyword_begin, Keyword::Begin);
make_keyword_fn!(keyword_bnot, Keyword::BinaryNot);
make_keyword_fn!(keyword_bor, Keyword::BinaryOr);
make_keyword_fn!(keyword_bsl, Keyword::BinaryShiftLeft);
make_keyword_fn!(keyword_bsr, Keyword::BinaryShiftRight);
make_keyword_fn!(keyword_bxor, Keyword::BinaryXor);
make_keyword_fn!(keyword_case, Keyword::Case);
make_keyword_fn!(keyword_catch, Keyword::Catch);
make_keyword_fn!(keyword_else, Keyword::Else);
make_keyword_fn!(keyword_end, Keyword::End);
make_keyword_fn!(keyword_fun, Keyword::Fun);
make_keyword_fn!(keyword_if, Keyword::If);
make_keyword_fn!(keyword_integerdiv, Keyword::IntegerDiv);
make_keyword_fn!(keyword_not, Keyword::Not);
make_keyword_fn!(keyword_of, Keyword::Of);
make_keyword_fn!(keyword_or, Keyword::Or);
make_keyword_fn!(keyword_orelse, Keyword::OrElse);
make_keyword_fn!(keyword_rem, Keyword::Rem);
make_keyword_fn!(keyword_try, Keyword::Try);
make_keyword_fn!(keyword_when, Keyword::When);
make_keyword_fn!(keyword_xor, Keyword::Xor);
