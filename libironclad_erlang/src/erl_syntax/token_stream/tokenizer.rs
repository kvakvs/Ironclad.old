//! Nom parser breaking input text into `ErlToken`s

use crate::erl_syntax::token_stream::keyword::Keyword;
use crate::erl_syntax::token_stream::misc::ws_before;
use crate::erl_syntax::token_stream::tok_strings::atom_literal::parse_tok_atom;
use crate::erl_syntax::token_stream::tok_strings::str_literal::parse_doublequot_string;
use crate::erl_syntax::token_stream::token::Token;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::char;
use nom::combinator::{map, not};
use nom::multi::many0;
use nom::Parser;

pub type TokInput<'a> = &'a str;
pub type TokResult<'a, Out> =
  nom::IResult<TokInput<'a>, Out, nom::error::VerboseError<TokInput<'a>>>;
/// Gathers multiple errors and contexts together
pub type TokenizerError<'a> = nom::error::VerboseError<TokInput<'a>>;

#[inline]
fn tok_atom(input: &str) -> TokResult<Token> {
  map(parse_tok_atom, |s| Token::Atom(s))(input)
}

#[inline]
fn symbol_comma(input: &str) -> TokResult<Token> {
  map(ws_before(char(',')), |_| Token::Comma)(input)
}

#[inline]
fn symbol_curlyclose(input: &str) -> TokResult<Token> {
  map(ws_before(char('}')), |_| Token::CurlyClose)(input)
}

#[inline]
fn symbol_curlyopen(input: &str) -> TokResult<Token> {
  map(ws_before(char('{')), |_| Token::CurlyOpen)(input)
}

#[inline]
fn symbol_div(input: &str) -> TokResult<Token> {
  map(ws_before(char('/').and(not(char('=')))), |_| Token::Div)(input)
}

#[inline]
fn symbol_doubleangleclose(input: &str) -> TokResult<Token> {
  map(ws_before(tag(">>")), |_| Token::DoubleAngleClose)(input)
}

#[inline]
fn symbol_doubleangleopen(input: &str) -> TokResult<Token> {
  map(ws_before(tag(">>")), |_| Token::DoubleAngleOpen)(input)
}

#[inline]
fn symbol_eq(input: &str) -> TokResult<Token> {
  map(ws_before(char(',')), |_| Token::EqualEqual)(input)
}

#[inline]
fn symbol_equalsymbol(input: &str) -> TokResult<Token> {
  map(ws_before(char('=').and(not(char('>')))), |_| Token::EqualSymbol)(input)
}

#[inline]
fn symbol_greatereq(input: &str) -> TokResult<Token> {
  map(ws_before(tag(">=")), |_| Token::GreaterEq)(input)
}

#[inline]
fn symbol_greaterthan(input: &str) -> TokResult<Token> {
  map(ws_before(char('>').and(not(char('=')))), |_| Token::GreaterThan)(input)
}

#[inline]
fn symbol_hardeq(input: &str) -> TokResult<Token> {
  map(ws_before(tag("=:=")), |_| Token::HardEq)(input)
}

#[inline]
fn symbol_equalequal(input: &str) -> TokResult<Token> {
  map(ws_before(tag("==")), |_| Token::HardEq)(input)
}

#[inline]
fn symbol_hardnoteq(input: &str) -> TokResult<Token> {
  map(ws_before(tag("=/=")), |_| Token::HardNotEq)(input)
}

#[inline]
fn symbol_leftarr(input: &str) -> TokResult<Token> {
  map(ws_before(tag("<-")), |_| Token::LeftArr)(input)
}

#[inline]
fn symbol_leftdoublearr(input: &str) -> TokResult<Token> {
  map(ws_before(tag("<=")), |_| Token::LeftDoubleArr)(input)
}

#[inline]
fn symbol_lessthan(input: &str) -> TokResult<Token> {
  // TODO? and not =, -, etc
  map(ws_before(char('<')), |_| Token::LessThan)(input)
}

#[inline]
fn symbol_lessthaneq(input: &str) -> TokResult<Token> {
  map(ws_before(tag("=<")), |_| Token::LessThanEq)(input)
}

#[inline]
fn symbol_listappend(input: &str) -> TokResult<Token> {
  map(ws_before(tag("++")), |_| Token::ListAppend)(input)
}

#[inline]
fn symbol_listsubtract(input: &str) -> TokResult<Token> {
  map(ws_before(tag("--")), |_| Token::ListSubtract)(input)
}

#[inline]
fn symbol_minus(input: &str) -> TokResult<Token> {
  map(ws_before(char('-')), |_| Token::Minus)(input)
}

#[inline]
fn symbol_mul(input: &str) -> TokResult<Token> {
  map(ws_before(char('*')), |_| Token::Mul)(input)
}

#[inline]
fn symbol_noteq(input: &str) -> TokResult<Token> {
  map(ws_before(tag("/=")), |_| Token::NotEq)(input)
}

#[inline]
fn symbol_parclose(input: &str) -> TokResult<Token> {
  map(ws_before(char(')')), |_| Token::ParClose)(input)
}

#[inline]
fn symbol_paropen(input: &str) -> TokResult<Token> {
  map(ws_before(char('(')), |_| Token::ParOpen)(input)
}

#[inline]
fn symbol_period(input: &str) -> TokResult<Token> {
  map(ws_before(char('.')), |_| Token::Period)(input)
}

#[inline]
fn symbol_hash(input: &str) -> TokResult<Token> {
  map(ws_before(char('#')), |_| Token::Period)(input)
}

#[inline]
fn symbol_plus(input: &str) -> TokResult<Token> {
  map(ws_before(char('+')), |_| Token::Plus)(input)
}

#[inline]
fn symbol_rightarr(input: &str) -> TokResult<Token> {
  map(ws_before(tag("->")), |_| Token::RightArr)(input)
}

#[inline]
fn symbol_rightdoublearr(input: &str) -> TokResult<Token> {
  map(ws_before(tag("=>")), |_| Token::RightDoubleArr)(input)
}

#[inline]
fn symbol_semicolon(input: &str) -> TokResult<Token> {
  map(ws_before(char(';')), |_| Token::Semicolon)(input)
}

#[inline]
fn symbol_send(input: &str) -> TokResult<Token> {
  map(ws_before(char('!')), |_| Token::Send)(input)
}

#[inline]
fn symbol_squareclose(input: &str) -> TokResult<Token> {
  map(ws_before(char(']')), |_| Token::SquareClose)(input)
}

#[inline]
fn symbol_squareopen(input: &str) -> TokResult<Token> {
  map(ws_before(char('[')), |_| Token::SquareOpen)(input)
}

#[inline]
fn tok_string(input: &str) -> TokResult<Token> {
  map(parse_doublequot_string, |s| Token::Str(s))(input)
}

fn tok_symbol(input: &str) -> TokResult<Token> {
  alt((
    alt((
      symbol_comma,            // ,
      symbol_curlyclose,       // }
      symbol_curlyopen,        // {
      symbol_div,              // /
      symbol_doubleangleclose, // >>
      symbol_doubleangleopen,  // <<
      symbol_greatereq,        // > =
      symbol_greaterthan,      // >
      symbol_leftarr,          // < -
    )),
    alt((
      symbol_leftdoublearr, // < =
      symbol_lessthan,      // <
      symbol_listappend,    // ++
      symbol_plus,          // +
      symbol_mul,           // *
      symbol_noteq,         // / =
      symbol_parclose,      // )
      symbol_paropen,       // (
      symbol_period,        // .
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
    symbol_hash,        // #
    symbol_semicolon,   // ;
    symbol_send,        // !
    symbol_squareclose, // ]
    symbol_squareopen,  // [
  ))(input)
}

#[inline]
fn keyword_after(input: &str) -> TokResult<Token> {
  map(ws_before(tag("after")), |_| Token::Keyword(Keyword::After))(input)
}
#[inline]
fn keyword_and(input: &str) -> TokResult<Token> {
  map(ws_before(tag("and")), |_| Token::Keyword(Keyword::And))(input)
}
#[inline]
fn keyword_andalso(input: &str) -> TokResult<Token> {
  map(ws_before(tag("andalso")), |_| Token::Keyword(Keyword::AndAlso))(input)
}
#[inline]
fn keyword_begin(input: &str) -> TokResult<Token> {
  map(ws_before(tag("begin")), |_| Token::Keyword(Keyword::Begin))(input)
}
#[inline]
fn keyword_binaryand(input: &str) -> TokResult<Token> {
  map(ws_before(tag("binaryand")), |_| Token::Keyword(Keyword::BinaryAnd))(input)
}
#[inline]
fn keyword_binarynot(input: &str) -> TokResult<Token> {
  map(ws_before(tag("binarynot")), |_| Token::Keyword(Keyword::BinaryNot))(input)
}
#[inline]
fn keyword_binaryor(input: &str) -> TokResult<Token> {
  map(ws_before(tag("binaryor")), |_| Token::Keyword(Keyword::BinaryOr))(input)
}
#[inline]
fn keyword_binaryshiftleft(input: &str) -> TokResult<Token> {
  map(ws_before(tag("binaryshiftleft")), |_| Token::Keyword(Keyword::BinaryShiftLeft))(input)
}
#[inline]
fn keyword_binaryshiftright(input: &str) -> TokResult<Token> {
  map(ws_before(tag("binaryshiftright")), |_| {
    Token::Keyword(Keyword::BinaryShiftRight)
  })(input)
}
#[inline]
fn keyword_binaryxor(input: &str) -> TokResult<Token> {
  map(ws_before(tag("binaryxor")), |_| Token::Keyword(Keyword::BinaryXor))(input)
}
#[inline]
fn keyword_case(input: &str) -> TokResult<Token> {
  map(ws_before(tag("case")), |_| Token::Keyword(Keyword::Case))(input)
}
#[inline]
fn keyword_catch(input: &str) -> TokResult<Token> {
  map(ws_before(tag("catch")), |_| Token::Keyword(Keyword::Catch))(input)
}
#[inline]
fn keyword_cond(input: &str) -> TokResult<Token> {
  map(ws_before(tag("cond")), |_| Token::Keyword(Keyword::Cond))(input)
}
#[inline]
fn keyword_end(input: &str) -> TokResult<Token> {
  map(ws_before(tag("end")), |_| Token::Keyword(Keyword::End))(input)
}
#[inline]
fn keyword_fun(input: &str) -> TokResult<Token> {
  map(ws_before(tag("fun")), |_| Token::Keyword(Keyword::Fun))(input)
}
#[inline]
fn keyword_if(input: &str) -> TokResult<Token> {
  map(ws_before(tag("if")), |_| Token::Keyword(Keyword::If))(input)
}
#[inline]
fn keyword_let(input: &str) -> TokResult<Token> {
  map(ws_before(tag("let")), |_| Token::Keyword(Keyword::Let))(input)
}
#[inline]
fn keyword_integerdiv(input: &str) -> TokResult<Token> {
  map(ws_before(tag("integerdiv")), |_| Token::Keyword(Keyword::IntegerDiv))(input)
}
#[inline]
fn keyword_maybe(input: &str) -> TokResult<Token> {
  map(ws_before(tag("maybe")), |_| Token::Keyword(Keyword::Maybe))(input)
}
#[inline]
fn keyword_not(input: &str) -> TokResult<Token> {
  map(ws_before(tag("not")), |_| Token::Keyword(Keyword::Not))(input)
}
#[inline]
fn keyword_of(input: &str) -> TokResult<Token> {
  map(ws_before(tag("of")), |_| Token::Keyword(Keyword::Of))(input)
}
#[inline]
fn keyword_or(input: &str) -> TokResult<Token> {
  map(ws_before(tag("or")), |_| Token::Keyword(Keyword::Or))(input)
}
#[inline]
fn keyword_orelse(input: &str) -> TokResult<Token> {
  map(ws_before(tag("orelse")), |_| Token::Keyword(Keyword::OrElse))(input)
}
#[inline]
fn keyword_receive(input: &str) -> TokResult<Token> {
  map(ws_before(tag("receive")), |_| Token::Keyword(Keyword::Receive))(input)
}
#[inline]
fn keyword_rem(input: &str) -> TokResult<Token> {
  map(ws_before(tag("rem")), |_| Token::Keyword(Keyword::Rem))(input)
}
#[inline]
fn keyword_try(input: &str) -> TokResult<Token> {
  map(ws_before(tag("try")), |_| Token::Keyword(Keyword::Try))(input)
}
#[inline]
fn keyword_when(input: &str) -> TokResult<Token> {
  map(ws_before(tag("when")), |_| Token::Keyword(Keyword::When))(input)
}
#[inline]
fn keyword_xor(input: &str) -> TokResult<Token> {
  map(ws_before(tag("xor")), |_| Token::Keyword(Keyword::Xor))(input)
}

fn tok_keyword(input: &str) -> TokResult<Token> {
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

pub fn tok_module(input: &str) -> TokResult<Vec<Token>> {
  many0(alt((
    //preprocessor_token,
    tok_string,
    tok_keyword,
    tok_atom,
    tok_symbol,
  )))(input)
}
