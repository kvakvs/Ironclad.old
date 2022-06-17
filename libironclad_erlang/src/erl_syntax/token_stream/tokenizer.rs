//! Nom parser breaking input text into `ErlToken`s

use crate::erl_syntax::token_stream::keyword::Keyword;
use crate::erl_syntax::token_stream::misc::{
  bigcapacity_many0, macro_ident, varname, ws_before_mut, ws_mut,
};
use crate::erl_syntax::token_stream::tok_input::{TokenizerInput, TokensResult};
use crate::erl_syntax::token_stream::tok_strings::atom_literal::parse_tok_atom;
use crate::erl_syntax::token_stream::tok_strings::str_literal::{
  parse_doublequot_string, parse_int,
};
use crate::erl_syntax::token_stream::tok_strings::Char;
use crate::erl_syntax::token_stream::token::Token;
use crate::erl_syntax::token_stream::token_type::TokenType;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{anychar, char};
use nom::combinator::{complete, cut, map, not, recognize};
use nom::error::context;
use nom::sequence::{preceded, terminated};
use nom::Parser;

#[inline]
fn tok_atom(input: TokenizerInput) -> TokensResult<Token> {
  map(parse_tok_atom, |s| Token::new(input.as_ptr(), TokenType::Atom(s)))(input)
}

#[inline]
fn tok_variable(input: TokenizerInput) -> TokensResult<Token> {
  map(varname, |v| Token::new(input.as_ptr(), TokenType::Variable(v)))(input)
}

#[inline]
fn tok_integer(input: TokenizerInput) -> TokensResult<Token> {
  map(parse_int, |i| Token::new(input.as_ptr(), TokenType::Integer(i)))(input)
}

#[inline]
fn symbol_comma(input: TokenizerInput) -> TokensResult<Token> {
  map(char(','), |_| Token::new(input.as_ptr(), TokenType::Comma))(input)
}

#[inline]
fn symbol_curlyclose(input: TokenizerInput) -> TokensResult<Token> {
  map(char('}'), |_| Token::new(input.as_ptr(), TokenType::CurlyClose))(input)
}

#[inline]
fn symbol_curlyopen(input: TokenizerInput) -> TokensResult<Token> {
  map(char('{'), |_| Token::new(input.as_ptr(), TokenType::CurlyOpen))(input)
}

#[inline]
fn symbol_div(input: TokenizerInput) -> TokensResult<Token> {
  map(char('/').and(not(char('='))), |_| Token::new(input.as_ptr(), TokenType::Div))(input)
}

#[inline]
fn symbol_doubleangleclose(input: TokenizerInput) -> TokensResult<Token> {
  map(tag(">>"), |_| Token::new(input.as_ptr(), TokenType::DoubleAngleClose))(input)
}

#[inline]
fn symbol_doubleangleopen(input: TokenizerInput) -> TokensResult<Token> {
  map(tag(">>"), |_| Token::new(input.as_ptr(), TokenType::DoubleAngleOpen))(input)
}

#[inline]
fn symbol_barbar(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("||"), |_| Token::new(input.as_ptr(), TokenType::BarBar))(input)
}

#[inline]
fn symbol_bar(input: TokenizerInput) -> TokensResult<Token> {
  map(char('|'), |_| Token::new(input.as_ptr(), TokenType::Bar))(input)
}

#[inline]
fn symbol_equalsymbol(input: TokenizerInput) -> TokensResult<Token> {
  map(char('=').and(not(char('>'))), |_| {
    Token::new(input.as_ptr(), TokenType::EqualSymbol)
  })(input)
}

#[inline]
fn symbol_greatereq(input: TokenizerInput) -> TokensResult<Token> {
  map(tag(">="), |_| Token::new(input.as_ptr(), TokenType::GreaterEq))(input)
}

#[inline]
fn symbol_greaterthan(input: TokenizerInput) -> TokensResult<Token> {
  map(char('>').and(not(char('='))), |_| {
    Token::new(input.as_ptr(), TokenType::GreaterThan)
  })(input)
}

#[inline]
fn symbol_hardeq(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("=:="), |_| Token::new(input.as_ptr(), TokenType::HardEq))(input)
}

#[inline]
fn symbol_equalequal(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("=="), |_| Token::new(input.as_ptr(), TokenType::HardEq))(input)
}

#[inline]
fn symbol_hardnoteq(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("=/="), |_| Token::new(input.as_ptr(), TokenType::HardNotEq))(input)
}

#[inline]
fn symbol_leftarr(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("<-"), |_| Token::new(input.as_ptr(), TokenType::LeftArr))(input)
}

#[inline]
fn symbol_leftdoublearr(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("<="), |_| Token::new(input.as_ptr(), TokenType::LeftDoubleArr))(input)
}

#[inline]
fn symbol_lessthan(input: TokenizerInput) -> TokensResult<Token> {
  // TODO? and not =, -, etc
  map(char('<'), |_| Token::new(input.as_ptr(), TokenType::LessThan))(input)
}

#[inline]
fn symbol_lessthaneq(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("=<"), |_| Token::new(input.as_ptr(), TokenType::LessThanEq))(input)
}

#[inline]
fn symbol_listappend(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("++"), |_| Token::new(input.as_ptr(), TokenType::ListAppend))(input)
}

#[inline]
fn symbol_listsubtract(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("--"), |_| Token::new(input.as_ptr(), TokenType::ListSubtract))(input)
}

#[inline]
fn symbol_minus(input: TokenizerInput) -> TokensResult<Token> {
  map(char('-'), |_| Token::new(input.as_ptr(), TokenType::Minus))(input)
}

#[inline]
fn symbol_mul(input: TokenizerInput) -> TokensResult<Token> {
  map(char('*'), |_| Token::new(input.as_ptr(), TokenType::Mul))(input)
}

#[inline]
fn symbol_noteq(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("/="), |_| Token::new(input.as_ptr(), TokenType::NotEq))(input)
}

#[inline]
fn symbol_parclose(input: TokenizerInput) -> TokensResult<Token> {
  map(char(')'), |_| Token::new(input.as_ptr(), TokenType::ParClose))(input)
}

#[inline]
fn symbol_paropen(input: TokenizerInput) -> TokensResult<Token> {
  map(char('('), |_| Token::new(input.as_ptr(), TokenType::ParOpen))(input)
}

#[inline]
fn symbol_periodperiod(input: TokenizerInput) -> TokensResult<Token> {
  map(tag(".."), |_| Token::new(input.as_ptr(), TokenType::PeriodPeriod))(input)
}

#[inline]
fn symbol_period(input: TokenizerInput) -> TokensResult<Token> {
  map(char('.'), |_| Token::new(input.as_ptr(), TokenType::Period))(input)
}

#[inline]
fn symbol_hash(input: TokenizerInput) -> TokensResult<Token> {
  map(char('#'), |_| Token::new(input.as_ptr(), TokenType::Period))(input)
}

#[inline]
fn symbol_plus(input: TokenizerInput) -> TokensResult<Token> {
  map(char('+'), |_| Token::new(input.as_ptr(), TokenType::Plus))(input)
}

#[inline]
fn symbol_rightarr(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("->"), |_| Token::new(input.as_ptr(), TokenType::RightArr))(input)
}

#[inline]
fn symbol_rightdoublearr(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("=>"), |_| Token::new(input.as_ptr(), TokenType::RightDoubleArr))(input)
}

#[inline]
fn symbol_semicolon(input: TokenizerInput) -> TokensResult<Token> {
  map(char(';'), |_| Token::new(input.as_ptr(), TokenType::Semicolon))(input)
}

#[inline]
fn symbol_coloncolon(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("::"), |_| Token::new(input.as_ptr(), TokenType::ColonColon))(input)
}

#[inline]
fn symbol_colon(input: TokenizerInput) -> TokensResult<Token> {
  map(char(':'), |_| Token::new(input.as_ptr(), TokenType::Colon))(input)
}

#[inline]
fn symbol_send(input: TokenizerInput) -> TokensResult<Token> {
  map(char('!'), |_| Token::new(input.as_ptr(), TokenType::Send))(input)
}

#[inline]
fn symbol_squareclose(input: TokenizerInput) -> TokensResult<Token> {
  map(char(']'), |_| Token::new(input.as_ptr(), TokenType::SquareClose))(input)
}

#[inline]
fn symbol_squareopen(input: TokenizerInput) -> TokensResult<Token> {
  map(char('['), |_| Token::new(input.as_ptr(), TokenType::SquareOpen))(input)
}

#[inline]
fn tok_string(input: TokenizerInput) -> TokensResult<Token> {
  map(parse_doublequot_string, |s| {
    Token::new(input.as_ptr(), TokenType::Str(s.into()))
  })(input)
}

#[inline]
fn dollar_character(input: TokenizerInput) -> TokensResult<Char> {
  map(recognize(alt((preceded(char('\\'), anychar), anychar))), |s: TokenizerInput| {
    s.chars().next().unwrap()
  })(input)
}

#[inline]
fn tok_dollar_character(input: TokenizerInput) -> TokensResult<Token> {
  map(preceded(char('$'), dollar_character), |c: Char| {
    Token::new(input.as_ptr(), TokenType::Character(c))
  })(input)
}

#[inline]
fn tok_macro_invocation(input: TokenizerInput) -> TokensResult<Token> {
  map(preceded(char('?'), context("macro invocation", cut(macro_ident))), |m| {
    Token::new(input.as_ptr(), TokenType::MacroInvocation(m))
  })(input)
}

fn tok_symbol(input: TokenizerInput) -> TokensResult<Token> {
  alt((
    alt((
      symbol_comma,            // ,
      symbol_curlyclose,       // }
      symbol_curlyopen,        // {
      symbol_div,              // /
      symbol_doubleangleclose, // >>
      symbol_greatereq,        // > =
      symbol_greaterthan,      // >
      symbol_periodperiod,     // ..
      symbol_period,           // .
      symbol_mul,              // *
    )),
    alt((
      symbol_leftarr,         // < -
      symbol_doubleangleopen, // <<
      symbol_leftdoublearr,   // < =
      symbol_lessthan,        // <
      symbol_listappend,      // ++
      symbol_plus,            // +
      symbol_noteq,           // / =
      symbol_parclose,        // )
      symbol_paropen,         // (
    )),
    alt((
      symbol_rightarr,       // - >
      symbol_listsubtract,   // - -
      symbol_minus,          // -
      symbol_rightdoublearr, // = >
      symbol_lessthaneq,     // = <
      symbol_equalequal,     // = =
      symbol_hardeq,         // =:=
      symbol_hardnoteq,      // =/=
      symbol_equalsymbol,    // =
    )),
    alt((
      symbol_hash,        // #
      symbol_barbar,      // ||
      symbol_bar,         // |
      symbol_semicolon,   // ;
      symbol_colon,       // :
      symbol_coloncolon,  // ::
      symbol_send,        // !
      symbol_squareclose, // ]
      symbol_squareopen,  // [
    )),
  ))(input)
}

#[inline]
fn keyword_after(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("after"), |_| Token::new_keyword(input.as_ptr(), Keyword::After))(input)
}

#[inline]
fn keyword_and(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("and"), |_| Token::new_keyword(input.as_ptr(), Keyword::And))(input)
}

#[inline]
fn keyword_andalso(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("andalso"), |_| Token::new_keyword(input.as_ptr(), Keyword::AndAlso))(input)
}

#[inline]
fn keyword_begin(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("begin"), |_| Token::new_keyword(input.as_ptr(), Keyword::Begin))(input)
}

#[inline]
fn keyword_binaryand(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("band"), |_| Token::new_keyword(input.as_ptr(), Keyword::BinaryAnd))(input)
}

#[inline]
fn keyword_binarynot(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("bnot"), |_| Token::new_keyword(input.as_ptr(), Keyword::BinaryNot))(input)
}

#[inline]
fn keyword_binaryor(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("bor"), |_| Token::new_keyword(input.as_ptr(), Keyword::BinaryOr))(input)
}

#[inline]
fn keyword_binaryshiftleft(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("bsl"), |_| Token::new_keyword(input.as_ptr(), Keyword::BinaryShiftLeft))(input)
}

#[inline]
fn keyword_binaryshiftright(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("bsr"), |_| Token::new_keyword(input.as_ptr(), Keyword::BinaryShiftRight))(input)
}

#[inline]
fn keyword_binaryxor(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("bxor"), |_| Token::new_keyword(input.as_ptr(), Keyword::BinaryXor))(input)
}

#[inline]
fn keyword_case(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("case"), |_| Token::new_keyword(input.as_ptr(), Keyword::Case))(input)
}

#[inline]
fn keyword_catch(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("catch"), |_| Token::new_keyword(input.as_ptr(), Keyword::Catch))(input)
}

#[inline]
fn keyword_cond(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("cond"), |_| Token::new_keyword(input.as_ptr(), Keyword::Cond))(input)
}

#[inline]
fn keyword_end(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("end"), |_| Token::new_keyword(input.as_ptr(), Keyword::End))(input)
}

#[inline]
fn keyword_fun(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("fun"), |_| Token::new_keyword(input.as_ptr(), Keyword::Fun))(input)
}

#[inline]
fn keyword_if(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("if"), |_| Token::new_keyword(input.as_ptr(), Keyword::If))(input)
}

#[inline]
fn keyword_let(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("let"), |_| Token::new_keyword(input.as_ptr(), Keyword::Let))(input)
}

#[inline]
fn keyword_integerdiv(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("div"), |_| Token::new_keyword(input.as_ptr(), Keyword::IntegerDiv))(input)
}

#[inline]
fn keyword_maybe(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("maybe"), |_| Token::new_keyword(input.as_ptr(), Keyword::Maybe))(input)
}

#[inline]
fn keyword_not(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("not"), |_| Token::new_keyword(input.as_ptr(), Keyword::Not))(input)
}

#[inline]
fn keyword_of(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("of"), |_| Token::new_keyword(input.as_ptr(), Keyword::Of))(input)
}

#[inline]
fn keyword_or(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("or"), |_| Token::new_keyword(input.as_ptr(), Keyword::Or))(input)
}

#[inline]
fn keyword_orelse(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("orelse"), |_| Token::new_keyword(input.as_ptr(), Keyword::OrElse))(input)
}

#[inline]
fn keyword_receive(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("receive"), |_| Token::new_keyword(input.as_ptr(), Keyword::Receive))(input)
}

#[inline]
fn keyword_rem(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("rem"), |_| Token::new_keyword(input.as_ptr(), Keyword::Rem))(input)
}

#[inline]
fn keyword_try(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("try"), |_| Token::new_keyword(input.as_ptr(), Keyword::Try))(input)
}

#[inline]
fn keyword_when(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("when"), |_| Token::new_keyword(input.as_ptr(), Keyword::When))(input)
}

#[inline]
fn keyword_else(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("else"), |_| Token::new_keyword(input.as_ptr(), Keyword::Else))(input)
}

#[inline]
fn keyword_xor(input: TokenizerInput) -> TokensResult<Token> {
  map(tag("xor"), |_| Token::new_keyword(input.as_ptr(), Keyword::Xor))(input)
}

#[inline]
fn tok_newline(input: TokenizerInput) -> TokensResult<Token> {
  map(alt((tag("\r\n"), tag("\n"), tag("\r"))), |_| {
    Token::new(input.as_ptr(), TokenType::Newline)
  })(input)
}

fn tok_keyword(input: TokenizerInput) -> TokensResult<Token> {
  alt((
    alt((
      keyword_after,
      keyword_and,
      keyword_andalso,
      keyword_begin,
      keyword_binaryand,
      keyword_binarynot,
      keyword_binaryor,
      keyword_binaryshiftleft,
      keyword_binaryshiftright,
    )),
    alt((
      keyword_binaryxor,
      keyword_case,
      keyword_catch,
      keyword_cond,
      keyword_end,
      keyword_fun,
      keyword_if,
      keyword_let,
      keyword_integerdiv,
    )),
    alt((
      keyword_maybe,
      keyword_not,
      keyword_of,
      keyword_or,
      keyword_orelse,
      keyword_receive,
      keyword_rem,
      keyword_try,
      keyword_when,
    )),
    keyword_xor,
  ))(input)
}

/// Break module source into tokens
pub fn tokenize_source(input: TokenizerInput) -> TokensResult<Vec<Token>> {
  // Comments after the code are consumed by the outer ws_mut
  // Comments and spaces between the tokens are consumed by the inner ws_before_mut
  complete(ws_mut(bigcapacity_many0(ws_before_mut(alt((
    //preprocessor_token,
    tok_newline,
    tok_macro_invocation,
    tok_dollar_character,
    tok_string,
    tok_keyword,
    tok_atom,
    tok_variable,
    tok_integer,
    tok_symbol,
  ))))))(input)
}