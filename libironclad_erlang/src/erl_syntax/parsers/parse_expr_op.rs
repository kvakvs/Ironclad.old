//! Parse an operator for binary or unary expressions
#![allow(missing_docs)]

use crate::erl_syntax::erl_op::{ErlBinaryOp, ErlUnaryOp};
use crate::erl_syntax::parsers::defs::{ParserInput, ParserResult};
use crate::erl_syntax::parsers::misc::{comma_tag, semicolon_tag};
use nom::branch::alt;
use nom::combinator::{map, not};
use nom::{bytes::complete::tag, character::complete::char, Parser};

type UnaryOpParserResult<'a> = ParserResult<'a, ErlUnaryOp>;
type BinaryOpParserResult<'a> = ParserResult<'a, ErlBinaryOp>;

pub fn unop_catch(input: ParserInput) -> UnaryOpParserResult {
  map(tag("catch".into()), |_| ErlUnaryOp::Catch)(input)
}

pub fn unop_not(input: ParserInput) -> UnaryOpParserResult {
  map(tag("not".into()), |_| ErlUnaryOp::Not)(input)
}

pub fn unop_bnot(input: ParserInput) -> UnaryOpParserResult {
  map(tag("bnot".into()), |_| ErlUnaryOp::BinaryNot)(input)
}

pub fn unop_positive(input: ParserInput) -> UnaryOpParserResult {
  map(char('+'), |_| ErlUnaryOp::Positive)(input)
}

pub fn unop_negative(input: ParserInput) -> UnaryOpParserResult {
  map(char('-'), |_| ErlUnaryOp::Negative)(input)
}

pub fn binop_floatdiv(input: ParserInput) -> BinaryOpParserResult {
  map(char('/').and(not(char('='))), |_| ErlBinaryOp::Div)(input)
}

pub fn binop_intdiv(input: ParserInput) -> BinaryOpParserResult {
  map(tag("div".into()), |_| ErlBinaryOp::IntegerDiv)(input)
}

pub fn binop_bang(input: ParserInput) -> BinaryOpParserResult {
  map(char('!'), |_| ErlBinaryOp::Bang)(input)
}

pub fn binop_multiply(input: ParserInput) -> BinaryOpParserResult {
  map(char('*'), |_| ErlBinaryOp::Mul)(input)
}

pub fn binop_add(input: ParserInput) -> BinaryOpParserResult {
  map(char('+').and(not(char('+'))), |_| ErlBinaryOp::Add)(input)
}

pub fn binop_subtract(input: ParserInput) -> BinaryOpParserResult {
  map(char('-').and(not(char('-'))), |_| ErlBinaryOp::Sub)(input)
}

pub fn binop_list_append(input: ParserInput) -> BinaryOpParserResult {
  map(tag("++".into()), |_| ErlBinaryOp::ListAppend)(input)
}

pub fn binop_list_subtract(input: ParserInput) -> BinaryOpParserResult {
  map(tag("--".into()), |_| ErlBinaryOp::ListSubtract)(input)
}

pub fn binop_rem(input: ParserInput) -> BinaryOpParserResult {
  map(tag("rem".into()), |_| ErlBinaryOp::Remainder)(input)
}

pub fn binop_and(input: ParserInput) -> BinaryOpParserResult {
  map(tag("and".into()), |_| ErlBinaryOp::And)(input)
}

pub fn binop_band(input: ParserInput) -> BinaryOpParserResult {
  map(tag("band".into()), |_| ErlBinaryOp::BinaryAnd)(input)
}

pub fn binop_or(input: ParserInput) -> BinaryOpParserResult {
  map(tag("or".into()), |_| ErlBinaryOp::Or)(input)
}

pub fn binop_orelse(input: ParserInput) -> BinaryOpParserResult {
  map(tag("orelse".into()), |_| ErlBinaryOp::OrElse)(input)
}

pub fn binop_andalso(input: ParserInput) -> BinaryOpParserResult {
  map(tag("andalso".into()), |_| ErlBinaryOp::AndAlso)(input)
}

pub fn binop_bor(input: ParserInput) -> BinaryOpParserResult {
  map(tag("bor".into()), |_| ErlBinaryOp::BinaryOr)(input)
}

pub fn binop_xor(input: ParserInput) -> BinaryOpParserResult {
  map(tag("xor".into()), |_| ErlBinaryOp::Xor)(input)
}

pub fn binop_bxor(input: ParserInput) -> BinaryOpParserResult {
  map(tag("bxor".into()), |_| ErlBinaryOp::BinaryXor)(input)
}

pub fn binop_bsl(input: ParserInput) -> BinaryOpParserResult {
  map(tag("bsl".into()), |_| ErlBinaryOp::BinaryShiftLeft)(input)
}

pub fn binop_bsr(input: ParserInput) -> BinaryOpParserResult {
  map(tag("bsr".into()), |_| ErlBinaryOp::BinaryShiftRight)(input)
}

pub fn binop_equals(input: ParserInput) -> BinaryOpParserResult {
  map(tag("==".into()), |_| ErlBinaryOp::Eq)(input)
}

pub fn binop_hard_equals(input: ParserInput) -> BinaryOpParserResult {
  map(tag("=:=".into()), |_| ErlBinaryOp::HardEq)(input)
}

pub fn binop_not_equals(input: ParserInput) -> BinaryOpParserResult {
  map(tag("/=".into()), |_| ErlBinaryOp::NotEq)(input)
}

pub fn binop_hard_not_equals(input: ParserInput) -> BinaryOpParserResult {
  map(tag("=/=".into()), |_| ErlBinaryOp::HardNotEq)(input)
}

pub fn binop_less(input: ParserInput) -> BinaryOpParserResult {
  map(char('<'), |_| ErlBinaryOp::Less)(input)
}

pub fn binop_less_eq(input: ParserInput) -> BinaryOpParserResult {
  map(tag("=<".into()), |_| ErlBinaryOp::LessEq)(input)
}

pub fn binop_greater(input: ParserInput) -> BinaryOpParserResult {
  map(char('>').and(not(char('='))), |_| ErlBinaryOp::Greater)(input)
}

pub fn binop_greater_eq(input: ParserInput) -> BinaryOpParserResult {
  map(tag(">=".into()), |_| ErlBinaryOp::GreaterEq)(input)
}

pub fn binop_match(input: ParserInput) -> BinaryOpParserResult {
  map(char('=').and(not(alt((char(':'), char('/'), char('<'))))), |_| {
    ErlBinaryOp::Match
  })(input)
}

pub fn binop_comma(input: ParserInput) -> BinaryOpParserResult {
  map(comma_tag, |_| ErlBinaryOp::Comma)(input)
}

pub fn binop_semicolon(input: ParserInput) -> BinaryOpParserResult {
  map(semicolon_tag, |_| ErlBinaryOp::Semicolon)(input)
}
