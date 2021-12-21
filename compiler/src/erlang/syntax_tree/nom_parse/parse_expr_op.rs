use std::sync::Arc;
use nom::{combinator, sequence, multi, branch,
          bytes::complete::{tag}};

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::erl_op::{ErlBinaryOp, ErlUnaryOp};
use crate::erlang::syntax_tree::node::erl_expression::{ErlBinaryOperatorExpr, ErlUnaryOperatorExpr};
use crate::erlang::syntax_tree::nom_parse::misc;
use crate::erlang::syntax_tree::nom_parse::parse_expr::parse_expr;
use crate::source_loc::SourceLoc;

fn unop_bang(input: &str) -> nom::IResult<&str, ErlUnaryOp> {
  combinator::map(tag("!"), |_| ErlUnaryOp::Bang)(input)
}
fn unop_catch(input: &str) -> nom::IResult<&str, ErlUnaryOp> {
  combinator::map(tag("!"), |_| ErlUnaryOp::Catch)(input)
}
fn unop_not(input: &str) -> nom::IResult<&str, ErlUnaryOp> {
  combinator::map(tag("not"), |_| ErlUnaryOp::Not)(input)
}
fn unop_bnot(input: &str) -> nom::IResult<&str, ErlUnaryOp> {
  combinator::map(tag("bnot"), |_| ErlUnaryOp::BinaryNot)(input)
}
fn unop_positive(input: &str) -> nom::IResult<&str, ErlUnaryOp> {
  combinator::map(tag("+"), |_| ErlUnaryOp::Positive)(input)
}
fn unop_negative(input: &str) -> nom::IResult<&str, ErlUnaryOp> {
  combinator::map(tag("-"), |_| ErlUnaryOp::Negative)(input)
}

fn parse_unary_op_symbol(input: &str) -> nom::IResult<&str, ErlUnaryOp> {
  branch::alt((
    unop_bang, unop_catch,
    unop_not, unop_bnot,
    unop_positive, unop_negative,
  ))(input)
}

fn binop_floatdiv(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("/"), |_| ErlBinaryOp::Div)(input)
}

fn binop_intdiv(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("div"), |_| ErlBinaryOp::IntegerDiv)(input)
}

fn binop_multiply(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("*"), |_| ErlBinaryOp::Mul)(input)
}

fn binop_add(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("+"), |_| ErlBinaryOp::Add)(input)
}

fn binop_subtract(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("-"), |_| ErlBinaryOp::Sub)(input)
}

fn binop_list_append(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("++"), |_| ErlBinaryOp::ListAppend)(input)
}

fn binop_list_subtract(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("--"), |_| ErlBinaryOp::ListSubtract)(input)
}

fn binop_rem(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("rem"), |_| ErlBinaryOp::Remainder)(input)
}

fn binop_and(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("and"), |_| ErlBinaryOp::And)(input)
}

fn binop_band(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("band"), |_| ErlBinaryOp::BinaryAnd)(input)
}

fn binop_or(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("or"), |_| ErlBinaryOp::Or)(input)
}

fn binop_orelse(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("orelse"), |_| ErlBinaryOp::OrElse)(input)
}

fn binop_andalso(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("andalso"), |_| ErlBinaryOp::AndAlso)(input)
}

fn binop_bor(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("bor"), |_| ErlBinaryOp::BinaryOr)(input)
}

fn binop_xor(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("xor"), |_| ErlBinaryOp::Xor)(input)
}

fn binop_bxor(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("bxor"), |_| ErlBinaryOp::BinaryXor)(input)
}

fn binop_bsl(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("bsl"), |_| ErlBinaryOp::BinaryShiftLeft)(input)
}

fn binop_bsr(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("bsr"), |_| ErlBinaryOp::BinaryShiftRight)(input)
}

fn binop_equals(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("=="), |_| ErlBinaryOp::Eq)(input)
}

fn binop_hard_equals(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("=:="), |_| ErlBinaryOp::HardEq)(input)
}

fn binop_not_equals(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("/="), |_| ErlBinaryOp::NotEq)(input)
}

fn binop_hard_not_equals(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("=/="), |_| ErlBinaryOp::HardNotEq)(input)
}

fn binop_less(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("<"), |_| ErlBinaryOp::Less)(input)
}

fn binop_less_eq(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("=<"), |_| ErlBinaryOp::LessEq)(input)
}

fn binop_greater(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag(">"), |_| ErlBinaryOp::Greater)(input)
}

fn binop_greater_eq(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag(">="), |_| ErlBinaryOp::GreaterEq)(input)
}

fn binop_match(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag("="), |_| ErlBinaryOp::Match)(input)
}

fn binop_comma(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag(","), |_| ErlBinaryOp::Comma)(input)
}

fn binop_semicolon(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  combinator::map(tag(";"), |_| ErlBinaryOp::Semicolon)(input)
}

fn parse_binop(input: &str) -> nom::IResult<&str, ErlBinaryOp> {
  branch::alt((
    // binop_floatdiv, binop_multiply, binop_intdiv, binop_rem,
    // binop_band, binop_and,
    // binop_list_append, binop_list_subtract,
    // binop_add, binop_subtract,
    // binop_bor, binop_bxor, binop_or, binop_xor,
    // binop_bsl, binop_bsr,
    // binop_equals, binop_not_equals, binop_hard_equals, binop_hard_not_equals,
    // binop_less, binop_greater, binop_less_eq, binop_greater_eq,
    // binop_match,
    // binop_comma, binop_semicolon,
    binop_andalso, binop_orelse,
  ))(input)
}

pub fn parse_unop_expr(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  combinator::map(
    sequence::tuple((
      misc::ws(parse_unary_op_symbol),
      parse_expr
  )), |(unop, expr)| {
      let uop = ErlUnaryOperatorExpr{ expr, operator: unop };
      ErlAst::UnaryOp(SourceLoc::None, uop).into()
    }
  )(input)
}

pub fn parse_binop_expr(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  combinator::map(
    sequence::tuple((
      parse_expr,
      misc::ws(parse_binop),
      parse_expr
    )),
    |(left, op, right)| {
      let bop_expr = ErlBinaryOperatorExpr {
        left,
        right,
        operator: op,
      };
      ErlAst::BinaryOp(SourceLoc::None, bop_expr).into()
    },
  )(input)
}
