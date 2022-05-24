//! Parse an operator for ironclad_exe or unary expressions
#![allow(missing_docs)]

use crate::syntax_tree::erl_op::{ErlBinaryOp, ErlUnaryOp};
use crate::syntax_tree::nom_parse::{ErlParser, ErlParserError};
use nom::branch::alt;
use nom::combinator::{map, not};
use nom::{bytes::complete::tag, character::complete::char, Parser};

type UnaryOpParserResult<'a> = nom::IResult<&'a str, ErlUnaryOp, ErlParserError<'a>>;
type BinaryOpParserResult<'a> = nom::IResult<&'a str, ErlBinaryOp, ErlParserError<'a>>;

impl ErlParser {
  pub fn unop_catch(input: &str) -> UnaryOpParserResult {
    map(tag("catch"), |_| ErlUnaryOp::Catch)(input)
  }

  pub fn unop_not(input: &str) -> UnaryOpParserResult {
    map(tag("not"), |_| ErlUnaryOp::Not)(input)
  }

  pub fn unop_bnot(input: &str) -> UnaryOpParserResult {
    map(tag("bnot"), |_| ErlUnaryOp::BinaryNot)(input)
  }

  pub fn unop_positive(input: &str) -> UnaryOpParserResult {
    map(char('+'), |_| ErlUnaryOp::Positive)(input)
  }

  pub fn unop_negative(input: &str) -> UnaryOpParserResult {
    map(char('-'), |_| ErlUnaryOp::Negative)(input)
  }

  pub fn binop_floatdiv(input: &str) -> BinaryOpParserResult {
    map(char('/').and(not(char('='))), |_| ErlBinaryOp::Div)(input)
  }

  pub fn binop_intdiv(input: &str) -> BinaryOpParserResult {
    map(tag("div"), |_| ErlBinaryOp::IntegerDiv)(input)
  }

  pub fn binop_bang(input: &str) -> BinaryOpParserResult {
    map(char('!'), |_| ErlBinaryOp::Bang)(input)
  }

  pub fn binop_multiply(input: &str) -> BinaryOpParserResult {
    map(char('*'), |_| ErlBinaryOp::Mul)(input)
  }

  pub fn binop_add(input: &str) -> BinaryOpParserResult {
    map(char('+').and(not(char('+'))), |_| ErlBinaryOp::Add)(input)
  }

  pub fn binop_subtract(input: &str) -> BinaryOpParserResult {
    map(char('-').and(not(char('-'))), |_| ErlBinaryOp::Sub)(input)
  }

  pub fn binop_list_append(input: &str) -> BinaryOpParserResult {
    map(tag("++"), |_| ErlBinaryOp::ListAppend)(input)
  }

  pub fn binop_list_subtract(input: &str) -> BinaryOpParserResult {
    map(tag("--"), |_| ErlBinaryOp::ListSubtract)(input)
  }

  pub fn binop_rem(input: &str) -> BinaryOpParserResult {
    map(tag("rem"), |_| ErlBinaryOp::Remainder)(input)
  }

  pub fn binop_and(input: &str) -> BinaryOpParserResult {
    map(tag("and"), |_| ErlBinaryOp::And)(input)
  }

  pub fn binop_band(input: &str) -> BinaryOpParserResult {
    map(tag("band"), |_| ErlBinaryOp::BinaryAnd)(input)
  }

  pub fn binop_or(input: &str) -> BinaryOpParserResult {
    map(tag("or"), |_| ErlBinaryOp::Or)(input)
  }

  pub fn binop_orelse(input: &str) -> BinaryOpParserResult {
    map(tag("orelse"), |_| ErlBinaryOp::OrElse)(input)
  }

  pub fn binop_andalso(input: &str) -> BinaryOpParserResult {
    map(tag("andalso"), |_| ErlBinaryOp::AndAlso)(input)
  }

  pub fn binop_bor(input: &str) -> BinaryOpParserResult {
    map(tag("bor"), |_| ErlBinaryOp::BinaryOr)(input)
  }

  pub fn binop_xor(input: &str) -> BinaryOpParserResult {
    map(tag("xor"), |_| ErlBinaryOp::Xor)(input)
  }

  pub fn binop_bxor(input: &str) -> BinaryOpParserResult {
    map(tag("bxor"), |_| ErlBinaryOp::BinaryXor)(input)
  }

  pub fn binop_bsl(input: &str) -> BinaryOpParserResult {
    map(tag("bsl"), |_| ErlBinaryOp::BinaryShiftLeft)(input)
  }

  pub fn binop_bsr(input: &str) -> BinaryOpParserResult {
    map(tag("bsr"), |_| ErlBinaryOp::BinaryShiftRight)(input)
  }

  pub fn binop_equals(input: &str) -> BinaryOpParserResult {
    map(tag("=="), |_| ErlBinaryOp::Eq)(input)
  }

  pub fn binop_hard_equals(input: &str) -> BinaryOpParserResult {
    map(tag("=:="), |_| ErlBinaryOp::HardEq)(input)
  }

  pub fn binop_not_equals(input: &str) -> BinaryOpParserResult {
    map(tag("/="), |_| ErlBinaryOp::NotEq)(input)
  }

  pub fn binop_hard_not_equals(input: &str) -> BinaryOpParserResult {
    map(tag("=/="), |_| ErlBinaryOp::HardNotEq)(input)
  }

  pub fn binop_less(input: &str) -> BinaryOpParserResult {
    map(char('<'), |_| ErlBinaryOp::Less)(input)
  }

  pub fn binop_less_eq(input: &str) -> BinaryOpParserResult {
    map(tag("=<"), |_| ErlBinaryOp::LessEq)(input)
  }

  pub fn binop_greater(input: &str) -> BinaryOpParserResult {
    map(char('>').and(not(char('='))), |_| ErlBinaryOp::Greater)(input)
  }

  pub fn binop_greater_eq(input: &str) -> BinaryOpParserResult {
    map(tag(">="), |_| ErlBinaryOp::GreaterEq)(input)
  }

  pub fn binop_match(input: &str) -> BinaryOpParserResult {
    map(tag("=").and(not(alt((char(':'), char('/'), char('<'))))), |_| {
      ErlBinaryOp::Match
    })(input)
  }

  pub fn binop_comma(input: &str) -> BinaryOpParserResult {
    map(tag(","), |_| ErlBinaryOp::Comma)(input)
  }

  pub fn binop_semicolon(input: &str) -> BinaryOpParserResult {
    map(tag(";"), |_| ErlBinaryOp::Semicolon)(input)
  }
}
