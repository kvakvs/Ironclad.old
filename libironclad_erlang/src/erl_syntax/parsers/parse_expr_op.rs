//! Parse an operator for binary or unary expressions
#![allow(missing_docs)]

use crate::erl_syntax::erl_op::{ErlBinaryOp, ErlUnaryOp};
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{
  tok, tok_comma, tok_forward_slash, tok_keyword, tok_minus, tok_semicolon, ws_before,
};
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::parsers::token_stream::keyword::Keyword;
use crate::erl_syntax::parsers::token_stream::token_type::TokenType;
use nom::combinator::map;

type UnaryOpParserResult<'a> = ParserResult<'a, ErlUnaryOp>;
type BinaryOpParserResult<'a> = ParserResult<'a, ErlBinaryOp>;

pub(crate) fn unop_catch(input: ParserInput) -> UnaryOpParserResult {
  map(ws_before(tok_keyword(Keyword::Catch)), |_| ErlUnaryOp::Catch)(input)
}

pub(crate) fn unop_not(input: ParserInput) -> UnaryOpParserResult {
  map(ws_before(tok_keyword(Keyword::Not)), |_| ErlUnaryOp::Not)(input)
}

pub(crate) fn unop_bnot(input: ParserInput) -> UnaryOpParserResult {
  map(ws_before(tok_keyword(Keyword::BinaryNot)), |_| ErlUnaryOp::BinaryNot)(input)
}

pub(crate) fn unop_positive(input: ParserInput) -> UnaryOpParserResult {
  map(ws_before(tok(TokenType::Plus)), |_| ErlUnaryOp::Positive)(input)
}

pub(crate) fn unop_negative(input: ParserInput) -> UnaryOpParserResult {
  map(tok_minus, |_| ErlUnaryOp::Negative)(input)
}

pub(crate) fn binop_floatdiv(input: ParserInput) -> BinaryOpParserResult {
  map(tok_forward_slash, |_| ErlBinaryOp::Div)(input)
}

pub(crate) fn binop_intdiv(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok_keyword(Keyword::IntegerDiv)), |_| ErlBinaryOp::IntegerDiv)(input)
}

pub(crate) fn binop_bang(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok(TokenType::Send)), |_| ErlBinaryOp::Send)(input)
}

pub(crate) fn binop_multiply(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok(TokenType::Asterisk)), |_| ErlBinaryOp::Mul)(input)
}

pub(crate) fn binop_add(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok(TokenType::Plus)), |_| ErlBinaryOp::Add)(input)
}

pub(crate) fn binop_subtract(input: ParserInput) -> BinaryOpParserResult {
  map(tok_minus, |_| ErlBinaryOp::Sub)(input)
}

pub(crate) fn binop_list_append(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok(TokenType::ListAppend)), |_| ErlBinaryOp::ListAppend)(input)
}

pub(crate) fn binop_list_subtract(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok(TokenType::ListSubtract)), |_| ErlBinaryOp::ListSubtract)(input)
}

pub(crate) fn binop_rem(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok_keyword(Keyword::Rem)), |_| ErlBinaryOp::Remainder)(input)
}

pub(crate) fn binop_and(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok_keyword(Keyword::And)), |_| ErlBinaryOp::And)(input)
}

pub(crate) fn binop_band(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok_keyword(Keyword::BinaryAnd)), |_| ErlBinaryOp::BinaryAnd)(input)
}

pub(crate) fn binop_or(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok_keyword(Keyword::Or)), |_| ErlBinaryOp::Or)(input)
}

pub(crate) fn binop_orelse(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok_keyword(Keyword::OrElse)), |_| ErlBinaryOp::OrElse)(input)
}

pub(crate) fn binop_andalso(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok_keyword(Keyword::AndAlso)), |_| ErlBinaryOp::AndAlso)(input)
}

pub(crate) fn binop_bor(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok_keyword(Keyword::BinaryOr)), |_| ErlBinaryOp::BinaryOr)(input)
}

pub(crate) fn binop_xor(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok_keyword(Keyword::Xor)), |_| ErlBinaryOp::Xor)(input)
}

pub(crate) fn binop_bxor(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok_keyword(Keyword::BinaryXor)), |_| ErlBinaryOp::BinaryXor)(input)
}

pub(crate) fn binop_bsl(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok_keyword(Keyword::BinaryShiftLeft)), |_| {
    ErlBinaryOp::BinaryShiftLeft
  })(input)
}

pub(crate) fn binop_bsr(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok_keyword(Keyword::BinaryShiftRight)), |_| {
    ErlBinaryOp::BinaryShiftRight
  })(input)
}

pub(crate) fn binop_equals(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok(TokenType::EqualEqual)), |_| ErlBinaryOp::Eq)(input)
}

pub(crate) fn binop_hard_equals(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok(TokenType::HardEq)), |_| ErlBinaryOp::HardEq)(input)
}

pub(crate) fn binop_not_equals(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok(TokenType::NotEq)), |_| ErlBinaryOp::NotEq)(input)
}

pub(crate) fn binop_hard_not_equals(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok(TokenType::HardNotEq)), |_| ErlBinaryOp::HardNotEq)(input)
}

pub(crate) fn binop_less(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok(TokenType::LessThan)), |_| ErlBinaryOp::Less)(input)
}

pub(crate) fn binop_less_eq(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok(TokenType::LessThanEq)), |_| ErlBinaryOp::LessEq)(input)
}

pub(crate) fn binop_greater(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok(TokenType::GreaterThan)), |_| ErlBinaryOp::Greater)(input)
}

pub(crate) fn binop_greater_eq(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok(TokenType::GreaterEq)), |_| ErlBinaryOp::GreaterEq)(input)
}

pub(crate) fn binop_match(input: ParserInput) -> BinaryOpParserResult {
  map(ws_before(tok(TokenType::EqualSymbol)), |_| ErlBinaryOp::Match)(input)
}

pub(crate) fn binop_comma(input: ParserInput) -> BinaryOpParserResult {
  map(tok_comma, |_| ErlBinaryOp::Comma)(input)
}

pub(crate) fn binop_semicolon(input: ParserInput) -> BinaryOpParserResult {
  map(tok_semicolon, |_| ErlBinaryOp::Semicolon)(input)
}
