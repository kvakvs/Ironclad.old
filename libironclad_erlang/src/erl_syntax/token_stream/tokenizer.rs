//! Nom parser breaking input text into `ErlToken`s

use crate::erl_syntax::token_stream::keyword::Keyword;
use crate::erl_syntax::token_stream::misc::{
  bigcapacity_many0, line_comment, macro_ident, varname, ws_before, ws_before_mut, ws_mut,
};
use crate::erl_syntax::token_stream::tok_strings::atom_literal::parse_tok_atom;
use crate::erl_syntax::token_stream::tok_strings::str_literal::{
  parse_doublequot_string, parse_int,
};
use crate::erl_syntax::token_stream::tok_strings::Char;
use crate::erl_syntax::token_stream::token::Token;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{anychar, char};
use nom::combinator::{complete, cut, map, not, recognize, verify};
use nom::error::context;
use nom::multi::many0;
use nom::sequence::{preceded, terminated};
use nom::Parser;

/// Tokenizer input type
pub type TokInput<'a> = &'a str;
/// Tokenizer parsers re
pub type TokResult<'a, Out> =
  nom::IResult<TokInput<'a>, Out, nom::error::VerboseError<TokInput<'a>>>;
/// Gathers multiple errors and contexts together
pub type TokenizerError<'a> = nom::error::VerboseError<TokInput<'a>>;

#[inline]
fn tok_atom(input: TokInput) -> TokResult<Token> {
  map(parse_tok_atom, |s| Token::Atom(s))(input)
}

#[inline]
fn tok_variable(input: TokInput) -> TokResult<Token> {
  map(varname, |v| Token::Variable(v))(input)
}

#[inline]
fn tok_integer(input: TokInput) -> TokResult<Token> {
  map(parse_int, |i| Token::Integer(i))(input)
}

#[inline]
fn symbol_comma(input: TokInput) -> TokResult<Token> {
  map(char(','), |_| Token::Comma)(input)
}

#[inline]
fn symbol_curlyclose(input: TokInput) -> TokResult<Token> {
  map(char('}'), |_| Token::CurlyClose)(input)
}

#[inline]
fn symbol_curlyopen(input: TokInput) -> TokResult<Token> {
  map(char('{'), |_| Token::CurlyOpen)(input)
}

#[inline]
fn symbol_div(input: TokInput) -> TokResult<Token> {
  map(char('/').and(not(char('='))), |_| Token::Div)(input)
}

#[inline]
fn symbol_doubleangleclose(input: TokInput) -> TokResult<Token> {
  map(tag(">>"), |_| Token::DoubleAngleClose)(input)
}

#[inline]
fn symbol_doubleangleopen(input: TokInput) -> TokResult<Token> {
  map(tag(">>"), |_| Token::DoubleAngleOpen)(input)
}

#[inline]
fn symbol_barbar(input: TokInput) -> TokResult<Token> {
  map(tag("||"), |_| Token::BarBar)(input)
}

#[inline]
fn symbol_bar(input: TokInput) -> TokResult<Token> {
  map(char('|'), |_| Token::Bar)(input)
}

#[inline]
fn symbol_equalsymbol(input: TokInput) -> TokResult<Token> {
  map(char('=').and(not(char('>'))), |_| Token::EqualSymbol)(input)
}

#[inline]
fn symbol_greatereq(input: TokInput) -> TokResult<Token> {
  map(tag(">="), |_| Token::GreaterEq)(input)
}

#[inline]
fn symbol_greaterthan(input: TokInput) -> TokResult<Token> {
  map(char('>').and(not(char('='))), |_| Token::GreaterThan)(input)
}

#[inline]
fn symbol_hardeq(input: TokInput) -> TokResult<Token> {
  map(tag("=:="), |_| Token::HardEq)(input)
}

#[inline]
fn symbol_equalequal(input: TokInput) -> TokResult<Token> {
  map(tag("=="), |_| Token::HardEq)(input)
}

#[inline]
fn symbol_hardnoteq(input: TokInput) -> TokResult<Token> {
  map(tag("=/="), |_| Token::HardNotEq)(input)
}

#[inline]
fn symbol_leftarr(input: TokInput) -> TokResult<Token> {
  map(tag("<-"), |_| Token::LeftArr)(input)
}

#[inline]
fn symbol_leftdoublearr(input: TokInput) -> TokResult<Token> {
  map(tag("<="), |_| Token::LeftDoubleArr)(input)
}

#[inline]
fn symbol_lessthan(input: TokInput) -> TokResult<Token> {
  // TODO? and not =, -, etc
  map(char('<'), |_| Token::LessThan)(input)
}

#[inline]
fn symbol_lessthaneq(input: TokInput) -> TokResult<Token> {
  map(tag("=<"), |_| Token::LessThanEq)(input)
}

#[inline]
fn symbol_listappend(input: TokInput) -> TokResult<Token> {
  map(tag("++"), |_| Token::ListAppend)(input)
}

#[inline]
fn symbol_listsubtract(input: TokInput) -> TokResult<Token> {
  map(tag("--"), |_| Token::ListSubtract)(input)
}

#[inline]
fn symbol_minus(input: TokInput) -> TokResult<Token> {
  map(char('-'), |_| Token::Minus)(input)
}

#[inline]
fn symbol_mul(input: TokInput) -> TokResult<Token> {
  map(char('*'), |_| Token::Mul)(input)
}

#[inline]
fn symbol_noteq(input: TokInput) -> TokResult<Token> {
  map(tag("/="), |_| Token::NotEq)(input)
}

#[inline]
fn symbol_parclose(input: TokInput) -> TokResult<Token> {
  map(char(')'), |_| Token::ParClose)(input)
}

#[inline]
fn symbol_paropen(input: TokInput) -> TokResult<Token> {
  map(char('('), |_| Token::ParOpen)(input)
}

#[inline]
fn symbol_period(input: TokInput) -> TokResult<Token> {
  map(char('.'), |_| Token::Period)(input)
}

#[inline]
fn symbol_hash(input: TokInput) -> TokResult<Token> {
  map(char('#'), |_| Token::Period)(input)
}

#[inline]
fn symbol_plus(input: TokInput) -> TokResult<Token> {
  map(char('+'), |_| Token::Plus)(input)
}

#[inline]
fn symbol_rightarr(input: TokInput) -> TokResult<Token> {
  map(tag("->"), |_| Token::RightArr)(input)
}

#[inline]
fn symbol_rightdoublearr(input: TokInput) -> TokResult<Token> {
  map(tag("=>"), |_| Token::RightDoubleArr)(input)
}

#[inline]
fn symbol_semicolon(input: TokInput) -> TokResult<Token> {
  map(char(';'), |_| Token::Semicolon)(input)
}

#[inline]
fn symbol_colon(input: TokInput) -> TokResult<Token> {
  map(char(':'), |_| Token::Colon)(input)
}

#[inline]
fn symbol_send(input: TokInput) -> TokResult<Token> {
  map(char('!'), |_| Token::Send)(input)
}

#[inline]
fn symbol_squareclose(input: TokInput) -> TokResult<Token> {
  map(char(']'), |_| Token::SquareClose)(input)
}

#[inline]
fn symbol_squareopen(input: TokInput) -> TokResult<Token> {
  map(char('['), |_| Token::SquareOpen)(input)
}

#[inline]
fn tok_string(input: TokInput) -> TokResult<Token> {
  map(parse_doublequot_string, |s| Token::Str(s))(input)
}

#[inline]
fn dollar_character(input: TokInput) -> TokResult<Char> {
  map(recognize(alt((preceded(char('\\'), anychar), anychar))), |s: TokInput| {
    s.chars().next().unwrap()
  })(input)
}

#[inline]
fn tok_dollar_character(input: TokInput) -> TokResult<Token> {
  map(preceded(char('$'), dollar_character), |c: Char| Token::Character(c))(input)
}

#[inline]
fn tok_macro_invocation(input: TokInput) -> TokResult<Token> {
  map(preceded(char('?'), context("macro invocation", cut(macro_ident))), |m| {
    Token::MacroInvocation(m)
  })(input)
}

fn tok_symbol(input: TokInput) -> TokResult<Token> {
  alt((
    alt((
      symbol_comma,            // ,
      symbol_curlyclose,       // }
      symbol_curlyopen,        // {
      symbol_div,              // /
      symbol_doubleangleclose, // >>
      symbol_greatereq,        // > =
      symbol_greaterthan,      // >
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
      symbol_send,        // !
      symbol_squareclose, // ]
      symbol_squareopen,  // [
    )),
  ))(input)
}

#[inline]
fn keyword_after(input: TokInput) -> TokResult<Token> {
  map(tag("after"), |_| Token::Keyword(Keyword::After))(input)
}

#[inline]
fn keyword_and(input: TokInput) -> TokResult<Token> {
  map(tag("and"), |_| Token::Keyword(Keyword::And))(input)
}

#[inline]
fn keyword_andalso(input: TokInput) -> TokResult<Token> {
  map(tag("andalso"), |_| Token::Keyword(Keyword::AndAlso))(input)
}

#[inline]
fn keyword_begin(input: TokInput) -> TokResult<Token> {
  map(tag("begin"), |_| Token::Keyword(Keyword::Begin))(input)
}

#[inline]
fn keyword_binaryand(input: TokInput) -> TokResult<Token> {
  map(tag("band"), |_| Token::Keyword(Keyword::BinaryAnd))(input)
}

#[inline]
fn keyword_binarynot(input: TokInput) -> TokResult<Token> {
  map(tag("bnot"), |_| Token::Keyword(Keyword::BinaryNot))(input)
}

#[inline]
fn keyword_binaryor(input: TokInput) -> TokResult<Token> {
  map(tag("bor"), |_| Token::Keyword(Keyword::BinaryOr))(input)
}

#[inline]
fn keyword_binaryshiftleft(input: TokInput) -> TokResult<Token> {
  map(tag("bsl"), |_| Token::Keyword(Keyword::BinaryShiftLeft))(input)
}

#[inline]
fn keyword_binaryshiftright(input: TokInput) -> TokResult<Token> {
  map(tag("bsr"), |_| Token::Keyword(Keyword::BinaryShiftRight))(input)
}

#[inline]
fn keyword_binaryxor(input: TokInput) -> TokResult<Token> {
  map(tag("bxor"), |_| Token::Keyword(Keyword::BinaryXor))(input)
}

#[inline]
fn keyword_case(input: TokInput) -> TokResult<Token> {
  map(tag("case"), |_| Token::Keyword(Keyword::Case))(input)
}

#[inline]
fn keyword_catch(input: TokInput) -> TokResult<Token> {
  map(tag("catch"), |_| Token::Keyword(Keyword::Catch))(input)
}

#[inline]
fn keyword_cond(input: TokInput) -> TokResult<Token> {
  map(tag("cond"), |_| Token::Keyword(Keyword::Cond))(input)
}

#[inline]
fn keyword_end(input: TokInput) -> TokResult<Token> {
  map(tag("end"), |_| Token::Keyword(Keyword::End))(input)
}

#[inline]
fn keyword_fun(input: TokInput) -> TokResult<Token> {
  map(tag("fun"), |_| Token::Keyword(Keyword::Fun))(input)
}

#[inline]
fn keyword_if(input: TokInput) -> TokResult<Token> {
  map(tag("if"), |_| Token::Keyword(Keyword::If))(input)
}

#[inline]
fn keyword_let(input: TokInput) -> TokResult<Token> {
  map(tag("let"), |_| Token::Keyword(Keyword::Let))(input)
}

#[inline]
fn keyword_integerdiv(input: TokInput) -> TokResult<Token> {
  map(tag("div"), |_| Token::Keyword(Keyword::IntegerDiv))(input)
}

#[inline]
fn keyword_maybe(input: TokInput) -> TokResult<Token> {
  map(tag("maybe"), |_| Token::Keyword(Keyword::Maybe))(input)
}

#[inline]
fn keyword_not(input: TokInput) -> TokResult<Token> {
  map(tag("not"), |_| Token::Keyword(Keyword::Not))(input)
}

#[inline]
fn keyword_of(input: TokInput) -> TokResult<Token> {
  map(tag("of"), |_| Token::Keyword(Keyword::Of))(input)
}

#[inline]
fn keyword_or(input: TokInput) -> TokResult<Token> {
  map(tag("or"), |_| Token::Keyword(Keyword::Or))(input)
}

#[inline]
fn keyword_orelse(input: TokInput) -> TokResult<Token> {
  map(tag("orelse"), |_| Token::Keyword(Keyword::OrElse))(input)
}

#[inline]
fn keyword_receive(input: TokInput) -> TokResult<Token> {
  map(tag("receive"), |_| Token::Keyword(Keyword::Receive))(input)
}

#[inline]
fn keyword_rem(input: TokInput) -> TokResult<Token> {
  map(tag("rem"), |_| Token::Keyword(Keyword::Rem))(input)
}

#[inline]
fn keyword_try(input: TokInput) -> TokResult<Token> {
  map(tag("try"), |_| Token::Keyword(Keyword::Try))(input)
}

#[inline]
fn keyword_when(input: TokInput) -> TokResult<Token> {
  map(tag("when"), |_| Token::Keyword(Keyword::When))(input)
}

#[inline]
fn keyword_xor(input: TokInput) -> TokResult<Token> {
  map(tag("xor"), |_| Token::Keyword(Keyword::Xor))(input)
}

// #[inline]
// fn tok_comment(input: TokInput) -> TokResult<Token> {
//   map(line_comment, |s| Token::Comment(s.to_string()))(input)
// }

fn tok_keyword(input: TokInput) -> TokResult<Token> {
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

pub fn tok_module(input: TokInput) -> TokResult<Vec<Token>> {
  // Comments after the code are consumed by the outer ws_mut
  // Comments and spaces between the tokens are consumed by the inner ws_before_mut
  complete(ws_mut(bigcapacity_many0(ws_before_mut(alt((
    //preprocessor_token,
    tok_macro_invocation,
    tok_dollar_character,
    tok_string,
    tok_keyword,
    tok_atom,
    tok_variable,
    tok_integer,
    tok_symbol,
    // tok_comment,
  ))))))(input)
}
