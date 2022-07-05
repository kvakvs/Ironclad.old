//! Parse an operator for binary or unary expressions
#![allow(missing_docs)]

use crate::erl_syntax::erl_op::{ErlBinaryOp, ErlUnaryOp};
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc_tok::*;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use nom::combinator::map;

type UnaryOpParserResult<'a> = ParserResult<'a, ErlUnaryOp>;
type BinaryOpParserResult<'a> = ParserResult<'a, ErlBinaryOp>;

pub(crate) fn unop_catch(input: ParserInput) -> UnaryOpParserResult {
  map(keyword_catch, |_| ErlUnaryOp::Catch)(input)
}

pub(crate) fn unop_not(input: ParserInput) -> UnaryOpParserResult {
  map(keyword_not, |_| ErlUnaryOp::Not)(input)
}

pub(crate) fn unop_bnot(input: ParserInput) -> UnaryOpParserResult {
  map(keyword_bnot, |_| ErlUnaryOp::BinaryNot)(input)
}

pub(crate) fn unop_positive(input: ParserInput) -> UnaryOpParserResult {
  map(tok_plus, |_| ErlUnaryOp::Positive)(input)
}

pub(crate) fn unop_negative(input: ParserInput) -> UnaryOpParserResult {
  map(tok_minus, |_| ErlUnaryOp::Negative)(input)
}

pub(crate) fn binop_floatdiv(input: ParserInput) -> BinaryOpParserResult {
  map(tok_forward_slash, |_| ErlBinaryOp::Div)(input)
}

pub(crate) fn binop_intdiv(input: ParserInput) -> BinaryOpParserResult {
  map(keyword_integerdiv, |_| ErlBinaryOp::IntegerDiv)(input)
}

pub(crate) fn binop_bang(input: ParserInput) -> BinaryOpParserResult {
  map(tok_send, |_| ErlBinaryOp::Send)(input)
}

pub(crate) fn binop_multiply(input: ParserInput) -> BinaryOpParserResult {
  map(tok_asterisk, |_| ErlBinaryOp::Mul)(input)
}

pub(crate) fn binop_add(input: ParserInput) -> BinaryOpParserResult {
  map(tok_plus, |_| ErlBinaryOp::Add)(input)
}

pub(crate) fn binop_subtract(input: ParserInput) -> BinaryOpParserResult {
  map(tok_minus, |_| ErlBinaryOp::Sub)(input)
}

pub(crate) fn binop_list_append(input: ParserInput) -> BinaryOpParserResult {
  map(tok_plus_plus, |_| ErlBinaryOp::ListAppend)(input)
}

pub(crate) fn binop_list_subtract(input: ParserInput) -> BinaryOpParserResult {
  map(tok_minus_minus, |_| ErlBinaryOp::ListSubtract)(input)
}

pub(crate) fn binop_rem(input: ParserInput) -> BinaryOpParserResult {
  map(keyword_rem, |_| ErlBinaryOp::Remainder)(input)
}

pub(crate) fn binop_and(input: ParserInput) -> BinaryOpParserResult {
  map(keyword_and, |_| ErlBinaryOp::And)(input)
}

pub(crate) fn binop_band(input: ParserInput) -> BinaryOpParserResult {
  map(keyword_band, |_| ErlBinaryOp::BinaryAnd)(input)
}

pub(crate) fn binop_or(input: ParserInput) -> BinaryOpParserResult {
  map(keyword_or, |_| ErlBinaryOp::Or)(input)
}

pub(crate) fn binop_orelse(input: ParserInput) -> BinaryOpParserResult {
  map(keyword_orelse, |_| ErlBinaryOp::OrElse)(input)
}

pub(crate) fn binop_andalso(input: ParserInput) -> BinaryOpParserResult {
  map(keyword_andalso, |_| ErlBinaryOp::AndAlso)(input)
}

pub(crate) fn binop_bor(input: ParserInput) -> BinaryOpParserResult {
  map(keyword_bor, |_| ErlBinaryOp::BinaryOr)(input)
}

pub(crate) fn binop_xor(input: ParserInput) -> BinaryOpParserResult {
  map(keyword_xor, |_| ErlBinaryOp::Xor)(input)
}

pub(crate) fn binop_bxor(input: ParserInput) -> BinaryOpParserResult {
  map(keyword_bxor, |_| ErlBinaryOp::BinaryXor)(input)
}

pub(crate) fn binop_bsl(input: ParserInput) -> BinaryOpParserResult {
  map(keyword_bsl, |_| ErlBinaryOp::BinaryShiftLeft)(input)
}

pub(crate) fn binop_bsr(input: ParserInput) -> BinaryOpParserResult {
  map(keyword_bsr, |_| ErlBinaryOp::BinaryShiftRight)(input)
}

pub(crate) fn binop_equals(input: ParserInput) -> BinaryOpParserResult {
  map(tok_equal_equal, |_| ErlBinaryOp::Eq)(input)
}

pub(crate) fn binop_hard_equals(input: ParserInput) -> BinaryOpParserResult {
  map(tok_hard_equal, |_| ErlBinaryOp::HardEq)(input)
}

pub(crate) fn binop_not_equals(input: ParserInput) -> BinaryOpParserResult {
  map(tok_not_equal, |_| ErlBinaryOp::NotEq)(input)
}

pub(crate) fn binop_hard_not_equals(input: ParserInput) -> BinaryOpParserResult {
  map(tok_hard_not_equal, |_| ErlBinaryOp::HardNotEq)(input)
}

pub(crate) fn binop_less(input: ParserInput) -> BinaryOpParserResult {
  map(tok_angle_open, |_| ErlBinaryOp::Less)(input)
}

pub(crate) fn binop_less_eq(input: ParserInput) -> BinaryOpParserResult {
  map(tok_less_eq, |_| ErlBinaryOp::LessEq)(input)
}

pub(crate) fn binop_greater(input: ParserInput) -> BinaryOpParserResult {
  map(tok_angle_close, |_| ErlBinaryOp::Greater)(input)
}

pub(crate) fn binop_greater_eq(input: ParserInput) -> BinaryOpParserResult {
  map(tok_greater_eq, |_| ErlBinaryOp::GreaterEq)(input)
}

pub(crate) fn binop_match(input: ParserInput) -> BinaryOpParserResult {
  map(tok_equal_symbol, |_| ErlBinaryOp::Match)(input)
}

pub(crate) fn binop_comma(input: ParserInput) -> BinaryOpParserResult {
  map(tok_comma, |_| ErlBinaryOp::Comma)(input)
}

pub(crate) fn binop_semicolon(input: ParserInput) -> BinaryOpParserResult {
  map(tok_semicolon, |_| ErlBinaryOp::Semicolon)(input)
}
