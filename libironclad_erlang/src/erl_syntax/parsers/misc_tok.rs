//! Token helpers

use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::ws_before;
use crate::erl_syntax::parsers::parser_error::ErlParserError;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::parsers::token_stream::keyword::Keyword;
use crate::erl_syntax::parsers::token_stream::token_kind::TokenKind;
use nom::combinator::map;
use nom::Slice;

#[inline]
fn void_fn<T>(_in: T) {}

/// Recognizes one token of a given tokentype, the tokentype fields are ignored.
/// *Complete version*: Will return an error if there's not enough input data.
fn tok(compare_val: TokenKind) -> impl Fn(ParserInput) -> ParserResult<()> {
  move |input: ParserInput| -> ParserResult<()> {
    match input.tokens.iter().next() {
      Some(tok) if tok.kind.is_same_type(&compare_val) => Ok((input.slice(1..), ())),
      other => Err(nom::Err::Error(ErlParserError::token_expected(
        input,
        compare_val.clone(),
        other
          .map(|t| t.kind.clone())
          .unwrap_or(TokenKind::EndOfInput),
      ))),
    }
  }
}

macro_rules! make_tok_fn {
  // Arguments are module name and function name of function to test bench
  ($tok_name:ident, TokenKind :: $tok_type:ident) => {
    // The macro will expand into the contents of this block.
    paste::item! {
        #[inline]
        pub(crate) fn [< $tok_name >] (input: ParserInput) -> ParserResult<()> {
            map(ws_before(tok(TokenKind :: $tok_type)), void_fn)(input)
        }
    }
  };
}

// - -- + ++ !
make_tok_fn!(tok_minus, TokenKind::Minus);
make_tok_fn!(tok_minus_minus, TokenKind::ListSubtract);
make_tok_fn!(tok_plus, TokenKind::Plus);
make_tok_fn!(tok_plus_plus, TokenKind::ListAppend);
make_tok_fn!(tok_send, TokenKind::Send);

// ( )
make_tok_fn!(tok_par_open, TokenKind::ParOpen);
make_tok_fn!(tok_par_close, TokenKind::ParClose);

// { }
make_tok_fn!(tok_curly_open, TokenKind::CurlyOpen);
make_tok_fn!(tok_curly_close, TokenKind::CurlyClose);

// << >>
make_tok_fn!(tok_double_angle_open, TokenKind::DoubleAngleOpen);
make_tok_fn!(tok_double_angle_close, TokenKind::DoubleAngleClose);

// # . / , ;
make_tok_fn!(tok_hash, TokenKind::Hash);
make_tok_fn!(tok_period, TokenKind::Period);
make_tok_fn!(tok_forward_slash, TokenKind::ForwardSlash);
make_tok_fn!(tok_comma, TokenKind::Comma);
make_tok_fn!(tok_semicolon, TokenKind::Semicolon);

// : :: | || ...
make_tok_fn!(tok_colon, TokenKind::Colon);
make_tok_fn!(tok_double_colon, TokenKind::ColonColon);
make_tok_fn!(tok_vertical_bar, TokenKind::VerticalBar);
make_tok_fn!(tok_double_vertical_bar, TokenKind::DoubleVerticalBar);
make_tok_fn!(tok_ellipsis, TokenKind::Ellipsis);

// * _ = == /=
make_tok_fn!(tok_asterisk, TokenKind::Asterisk);
make_tok_fn!(tok_underscore, TokenKind::Underscore);
make_tok_fn!(tok_equal_symbol, TokenKind::EqualSymbol);
make_tok_fn!(tok_equal_equal, TokenKind::EqualEqual);
make_tok_fn!(tok_not_equal, TokenKind::NotEq);

// < > =< >=
make_tok_fn!(tok_angle_open, TokenKind::AngleOpen);
make_tok_fn!(tok_angle_close, TokenKind::AngleClose);
make_tok_fn!(tok_less_eq, TokenKind::LessThanEq);
make_tok_fn!(tok_greater_eq, TokenKind::GreaterEq);

// =/= =:= .. => :=
make_tok_fn!(tok_hard_not_equal, TokenKind::HardNotEq);
make_tok_fn!(tok_hard_equal, TokenKind::HardEq);
make_tok_fn!(tok_double_period, TokenKind::PeriodPeriod);
make_tok_fn!(tok_right_darr, TokenKind::RightDoubleArr);
make_tok_fn!(tok_assign, TokenKind::Assign);

// -> <-- [ ] \n
make_tok_fn!(tok_left_arrow, TokenKind::LeftArr);
make_tok_fn!(tok_right_arrow, TokenKind::RightArr);
make_tok_fn!(tok_square_open, TokenKind::SquareOpen);
make_tok_fn!(tok_square_close, TokenKind::SquareClose);

/// Match an end of line token
#[inline]
pub fn tok_eol(input: ParserInput) -> ParserResult<()> {
  map(tok(TokenKind::EOL), void_fn)(input)
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
