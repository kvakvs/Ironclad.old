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

pub(crate) fn unop_catch(input: ParserInput) -> UnaryOpParserResult {
  map(tag("catch".into()), |_| ErlUnaryOp::Catch)(input)
}

pub(crate) fn unop_not(input: ParserInput) -> UnaryOpParserResult {
  map(tag("not".into()), |_| ErlUnaryOp::Not)(input)
}

pub(crate) fn unop_bnot(input: ParserInput) -> UnaryOpParserResult {
  map(tag("bnot".into()), |_| ErlUnaryOp::BinaryNot)(input)
}

pub(crate) fn unop_positive(input: ParserInput) -> UnaryOpParserResult {
  map(char('+'), |_| ErlUnaryOp::Positive)(input)
}

pub(crate) fn unop_negative(input: ParserInput) -> UnaryOpParserResult {
  map(char('-'), |_| ErlUnaryOp::Negative)(input)
}

pub(crate) fn binop_floatdiv(input: ParserInput) -> BinaryOpParserResult {
  map(char('/').and(not(char('='))), |_| ErlBinaryOp::Div)(input)
}

pub(crate) fn binop_intdiv(input: ParserInput) -> BinaryOpParserResult {
  map(tag("div".into()), |_| ErlBinaryOp::IntegerDiv)(input)
}

pub(crate) fn binop_bang(input: ParserInput) -> BinaryOpParserResult {
  map(char('!'), |_| ErlBinaryOp::Bang)(input)
}

pub(crate) fn binop_multiply(input: ParserInput) -> BinaryOpParserResult {
  map(char('*'), |_| ErlBinaryOp::Mul)(input)
}

pub(crate) fn binop_add(input: ParserInput) -> BinaryOpParserResult {
  map(char('+').and(not(char('+'))), |_| ErlBinaryOp::Add)(input)
}

pub(crate) fn binop_subtract(input: ParserInput) -> BinaryOpParserResult {
  map(char('-').and(not(char('-'))), |_| ErlBinaryOp::Sub)(input)
}

pub(crate) fn binop_list_append(input: ParserInput) -> BinaryOpParserResult {
  map(tag("++".into()), |_| ErlBinaryOp::ListAppend)(input)
}

pub(crate) fn binop_list_subtract(input: ParserInput) -> BinaryOpParserResult {
  map(tag("--".into()), |_| ErlBinaryOp::ListSubtract)(input)
}

pub(crate) fn binop_rem(input: ParserInput) -> BinaryOpParserResult {
  map(tag("rem".into()), |_| ErlBinaryOp::Remainder)(input)
}

pub(crate) fn binop_and(input: ParserInput) -> BinaryOpParserResult {
  map(tag("and".into()), |_| ErlBinaryOp::And)(input)
}

pub(crate) fn binop_band(input: ParserInput) -> BinaryOpParserResult {
  map(tag("band".into()), |_| ErlBinaryOp::BinaryAnd)(input)
}

pub(crate) fn binop_or(input: ParserInput) -> BinaryOpParserResult {
  map(tag("or".into()), |_| ErlBinaryOp::Or)(input)
}

pub(crate) fn binop_orelse(input: ParserInput) -> BinaryOpParserResult {
  map(tag("orelse".into()), |_| ErlBinaryOp::OrElse)(input)
}

pub(crate) fn binop_andalso(input: ParserInput) -> BinaryOpParserResult {
  map(tag("andalso".into()), |_| ErlBinaryOp::AndAlso)(input)
}

pub(crate) fn binop_bor(input: ParserInput) -> BinaryOpParserResult {
  map(tag("bor".into()), |_| ErlBinaryOp::BinaryOr)(input)
}

pub(crate) fn binop_xor(input: ParserInput) -> BinaryOpParserResult {
  map(tag("xor".into()), |_| ErlBinaryOp::Xor)(input)
}

pub(crate) fn binop_bxor(input: ParserInput) -> BinaryOpParserResult {
  map(tag("bxor".into()), |_| ErlBinaryOp::BinaryXor)(input)
}

pub(crate) fn binop_bsl(input: ParserInput) -> BinaryOpParserResult {
  map(tag("bsl".into()), |_| ErlBinaryOp::BinaryShiftLeft)(input)
}

pub(crate) fn binop_bsr(input: ParserInput) -> BinaryOpParserResult {
  map(tag("bsr".into()), |_| ErlBinaryOp::BinaryShiftRight)(input)
}

pub(crate) fn binop_equals(input: ParserInput) -> BinaryOpParserResult {
  map(tag("==".into()), |_| ErlBinaryOp::Eq)(input)
}

pub(crate) fn binop_hard_equals(input: ParserInput) -> BinaryOpParserResult {
  map(tag("=:=".into()), |_| ErlBinaryOp::HardEq)(input)
}

pub(crate) fn binop_not_equals(input: ParserInput) -> BinaryOpParserResult {
  map(tag("/=".into()), |_| ErlBinaryOp::NotEq)(input)
}

pub(crate) fn binop_hard_not_equals(input: ParserInput) -> BinaryOpParserResult {
  map(tag("=/=".into()), |_| ErlBinaryOp::HardNotEq)(input)
}

pub(crate) fn binop_less(input: ParserInput) -> BinaryOpParserResult {
  map(char('<'), |_| ErlBinaryOp::Less)(input)
}

pub(crate) fn binop_less_eq(input: ParserInput) -> BinaryOpParserResult {
  map(tag("=<".into()), |_| ErlBinaryOp::LessEq)(input)
}

pub(crate) fn binop_greater(input: ParserInput) -> BinaryOpParserResult {
  map(char('>').and(not(char('='))), |_| ErlBinaryOp::Greater)(input)
}

pub(crate) fn binop_greater_eq(input: ParserInput) -> BinaryOpParserResult {
  map(tag(">=".into()), |_| ErlBinaryOp::GreaterEq)(input)
}

pub(crate) fn binop_match(input: ParserInput) -> BinaryOpParserResult {
  map(char('=').and(not(alt((char(':'), char('/'), char('<'))))), |_| {
    ErlBinaryOp::Match
  })(input)
}

pub(crate) fn binop_comma(input: ParserInput) -> BinaryOpParserResult {
  map(comma_tag, |_| ErlBinaryOp::Comma)(input)
}

pub(crate) fn binop_semicolon(input: ParserInput) -> BinaryOpParserResult {
  map(semicolon_tag, |_| ErlBinaryOp::Semicolon)(input)
}
