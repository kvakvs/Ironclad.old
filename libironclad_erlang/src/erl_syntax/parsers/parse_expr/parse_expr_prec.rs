//! Precedence parser for expressions.

use crate::erl_syntax::erl_ast::expr_style::ExprStyle;
use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::erl_op::{ErlBinaryOp, ErlUnaryOp};
use crate::erl_syntax::node::erl_binop::ErlBinaryOperatorExpr;
use crate::erl_syntax::node::erl_callable_target::CallableTarget;
use crate::erl_syntax::node::erl_unop::ErlUnaryOperatorExpr;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::lang_construct::LangConstruct;
use crate::erl_syntax::parsers::misc;
use crate::erl_syntax::parsers::misc::tok_colon;
use crate::erl_syntax::parsers::parse_binary::parse_binary;
use crate::erl_syntax::parsers::parse_case::parse_case_expression;
use crate::erl_syntax::parsers::parse_expr::parse_expr_list::{
  parse_list_builder, parse_list_comprehension,
};
use crate::erl_syntax::parsers::parse_expr::parse_expr_map::parse_map_builder_no_base;
use crate::erl_syntax::parsers::parse_expr::parse_expr_record::{
  parse_record_builder_no_base, parse_record_field_access_no_base,
};
use crate::erl_syntax::parsers::parse_expr::{
  parenthesized_expr, parse_begin_end, parse_binary_comprehension, parse_fn_reference,
  parse_parenthesized_list_of_exprs, parse_tuple_builder, parse_var,
};
use crate::erl_syntax::parsers::parse_expr_op::{
  binop_add, binop_and, binop_andalso, binop_band, binop_bang, binop_bor, binop_bsl, binop_bsr,
  binop_bxor, binop_comma, binop_equals, binop_floatdiv, binop_greater, binop_greater_eq,
  binop_hard_equals, binop_hard_not_equals, binop_intdiv, binop_less, binop_less_eq,
  binop_list_append, binop_list_subtract, binop_match, binop_multiply, binop_not_equals, binop_or,
  binop_orelse, binop_rem, binop_semicolon, binop_subtract, binop_xor, unop_bnot, unop_catch,
  unop_negative, unop_not, unop_positive,
};
use crate::erl_syntax::parsers::parse_fn::parse_lambda;
use crate::erl_syntax::parsers::parse_if_stmt::parse_if_expression;
use crate::erl_syntax::parsers::parse_lit::parse_erl_literal;
use crate::erl_syntax::parsers::parse_try_catch::parse_try_catch;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::source_loc::SourceLoc;
use nom::branch::alt;
use nom::combinator::{consumed, map, opt};
use nom::error::context;
use nom::multi::many0;
use nom::sequence::{pair, preceded, tuple};
use nom::Parser;

/// Priority 0: (Parenthesized expressions), numbers, variables, negation (unary ops)
fn parse_expr_prec_primary<'a>(input: ParserInput<'a>) -> ParserResult<AstNode> {
  let alt_failed = |i: ParserInput<'a>| -> ParserResult<AstNode> {
    misc::alt_failed(
      i,
      "expression",
      &[
        LangConstruct::Lambda,
        LangConstruct::BeginEnd,
        LangConstruct::TryCatch,
        LangConstruct::IfExpression,
        LangConstruct::CaseExpression,
        LangConstruct::ParenthesizedExpression,
        LangConstruct::List,
        LangConstruct::Tuple,
        LangConstruct::FunctionReference,
        LangConstruct::Map,
        LangConstruct::Record,
        LangConstruct::Variable,
        LangConstruct::Literal,
        LangConstruct::ListComprehension,
        LangConstruct::BinaryComprehension,
        LangConstruct::Binary,
      ],
    )
  };
  context(
    "[hidden] parse expression (highest precedence)",
    alt((
      alt((
        parse_lambda,
        parse_begin_end,
        parse_try_catch,
        parse_if_expression,
        parse_case_expression,
        parenthesized_expr,
        parse_list_builder,
        parse_tuple_builder,
      )),
      alt((
        parse_fn_reference,
        parse_map_builder_no_base,
        parse_record_builder_no_base,
        parse_var,
        parse_erl_literal,
        parse_list_comprehension,
        parse_binary_comprehension,
        parse_binary,
      )),
    ))
    .or(alt_failed),
  )(input)
}

// TODO: Precedence 1: : (colon operator, for bit fields and module access?)
// TODO: module:function notation and maybe tuple notation?
/// Parse expr followed by a parentheses with 0 or more args, to become a function call
fn parse_expr_prec01(input: ParserInput) -> ParserResult<AstNode> {
  map(
    tuple((
      parse_expr_prec_primary,
      // An optional second expression after a ':', MUST be followed by parentheses with args
      opt(
        // A pair: optional second expr for function name, and mandatory args
        pair(
          opt(preceded(tok_colon, parse_expr_prec_primary)),
          parse_parenthesized_list_of_exprs,
        ),
      ),
    )),
    |(expr1, maybe_expr2_args)| {
      if let Some((maybe_expr2, args)) = maybe_expr2_args {
        match maybe_expr2 {
          Some(expr2) => {
            // TODO: merge match clause 2 and 3 as new_mfa_expr should be doing job of both?
            let target = CallableTarget::new_mfa_expr(Some(expr1), expr2, args.len());
            AstNodeImpl::new_application(SourceLoc::new(&input), target, args)
          }
          None => {
            let target = CallableTarget::new_expr(expr1);
            AstNodeImpl::new_application(SourceLoc::new(&input), target, args)
          }
        }
      } else {
        expr1 // no extra args after the expression
      }
    },
  )(input.clone())
}

// TODO: Precedence 2: # (record/map access operator)
#[inline]
fn parse_expr_prec02(input: ParserInput) -> ParserResult<AstNode> {
  let set_base = |(base, node): (AstNode, AstNode)| -> AstNode { node.set_base(Some(base)) };
  map(
    pair(
      parse_expr_prec01,
      alt((
        parse_record_field_access_no_base,
        parse_record_builder_no_base,
        parse_map_builder_no_base,
      )),
    ),
    set_base,
  )(input.clone())
  .or_else(|_err| parse_expr_prec01(input.clone()))
}

/// Precedence 3: Unary + - bnot not
fn parse_expr_prec03(input: ParserInput) -> ParserResult<AstNode> {
  map(
    consumed(pair(
      alt((unop_negative, unop_positive, unop_bnot, unop_not)),
      parse_expr_prec02,
    )),
    mk_unary,
  )(input.clone())
  .or_else(|_err| parse_expr_prec02(input.clone()))
}

/// Precedence 4: / * div rem band and, left associative
fn parse_expr_prec04(input: ParserInput) -> ParserResult<AstNode> {
  map(
    // Higher precedence expr, followed by 0 or more operators and higher prec exprs
    consumed(pair(
      parse_expr_prec03,
      many0(pair(
        alt((binop_floatdiv, binop_multiply, binop_intdiv, binop_rem, binop_band, binop_and)),
        parse_expr_prec03,
      )),
    )),
    mk_binop_left_assoc,
  )(input)
}

/// Precedence 5: + - bor bxor bsl bsr or xor, left associative
fn parse_expr_prec05(input: ParserInput) -> ParserResult<AstNode> {
  map(
    // Higher precedence expr, followed by 0 or more operators and higher prec exprs
    consumed(tuple((
      parse_expr_prec04,
      many0(pair(
        alt((
          binop_add,
          binop_bor,
          binop_bsl,
          binop_bsr,
          binop_bxor,
          binop_or,
          binop_subtract,
          binop_xor,
        )),
        parse_expr_prec04,
      )),
    ))),
    mk_binop_left_assoc,
  )(input)
}

/// Precedence 6: ++ --, right associative
fn parse_expr_prec06(input: ParserInput) -> ParserResult<AstNode> {
  map(
    // Higher precedence expr, followed by 0 or more operators and higher prec exprs
    consumed(pair(
      parse_expr_prec05,
      many0(pair(alt((binop_list_append, binop_list_subtract)), parse_expr_prec05)),
    )),
    mk_binop_right_assoc,
  )(input)
}

/// Precedence 7: == /= =< < >= > =:= =/=
fn parse_expr_prec07(input: ParserInput) -> ParserResult<AstNode> {
  map(
    // Higher precedence expr, followed by 0 or more operators and higher prec exprs
    consumed(pair(
      parse_expr_prec06,
      many0(pair(
        alt((
          binop_hard_equals,
          binop_hard_not_equals,
          binop_not_equals,
          binop_equals,
          binop_less_eq,
          binop_less,
          binop_greater_eq,
          binop_greater,
        )),
        parse_expr_prec06,
      )),
    )),
    mk_binop_left_assoc,
  )(input)
}

/// Precedence 8: andalso
fn parse_expr_prec08(input: ParserInput) -> ParserResult<AstNode> {
  map(
    // Higher precedence expr, followed by 0 or more ANDALSO operators and higher prec exprs
    consumed(pair(parse_expr_prec07, many0(pair(binop_andalso, parse_expr_prec07)))),
    mk_binop_left_assoc,
  )(input)
}

#[inline]
/// Call this with result of nom's `consumed()` combinator
fn mk_binop_left_assoc(
  (consumed_input, (left, tail)): (ParserInput, (AstNode, Vec<(ErlBinaryOp, AstNode)>)),
) -> AstNode {
  ErlBinaryOperatorExpr::new_left_assoc(SourceLoc::new(&consumed_input), left, &tail)
}

/// Precedence 9: orelse
fn parse_expr_prec09(input: ParserInput) -> ParserResult<AstNode> {
  map(
    // Higher precedence expr, followed by 0 or more ORELSE operators and higher prec exprs
    consumed(pair(parse_expr_prec08, many0(pair(binop_orelse, parse_expr_prec08)))),
    mk_binop_left_assoc,
  )(input)
}

#[inline]
fn mk_binop_right_assoc(
  (consumed_input, (left, tail)): (ParserInput, (AstNode, Vec<(ErlBinaryOp, AstNode)>)),
) -> AstNode {
  ErlBinaryOperatorExpr::new_right_assoc(SourceLoc::new(&consumed_input), left, &tail)
}

/// Precedence 10: assignment/match = operator, and send operator "!", right associative
fn parse_expr_prec10(input: ParserInput) -> ParserResult<AstNode> {
  map(
    // Higher precedence expr, followed by 0 or more binary operators and higher prec exprs
    consumed(pair(
      parse_expr_prec09,
      many0(pair(alt((binop_match, binop_bang)), parse_expr_prec09)),
    )),
    mk_binop_right_assoc,
  )(input)
}

#[inline]
/// Wrap values passed here with `consumed()` nom combinator
fn mk_unary((consumed_input, (catch_op, expr)): (ParserInput, (ErlUnaryOp, AstNode))) -> AstNode {
  ErlUnaryOperatorExpr::new_ast(SourceLoc::new(&consumed_input), catch_op, expr)
}

/// Precedence 11: Catch operator, then continue to higher precedences
/// This is also entry point to parse expression when you don't want to recognize comma and semicolon
fn parse_expr_prec11(input: ParserInput) -> ParserResult<AstNode> {
  // Try parse (catch Expr) otherwise try next precedence level
  map(consumed(pair(unop_catch, parse_expr_prec10)), mk_unary)(input.clone())
    .or_else(|_err| parse_expr_prec10(input.clone()))
}

/// Lowest precedence 13, where we handle comma and semicolon as binary ops.
/// Note that semicolon is not valid for regular code only allowed in guards.
fn parse_expr_prec13(style: ExprStyle, input: ParserInput) -> ParserResult<AstNode> {
  // Only guard style expressions allow comma/semicolon as binary ops
  if style == ExprStyle::Guard {
    // Guard-style expressions allow both comma and semicolons
    map(
      // Higher precedence expr, followed by 0 or more binary operators and higher prec exprs
      pair(
        parse_expr_prec11,
        many0(pair(alt((binop_semicolon, binop_comma)), parse_expr_prec11)),
      ),
      |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(SourceLoc::None, left, &tail),
    )(input)
  } else {
    parse_expr_prec11(input)
  }
}

/// Parse an expression from the beginning of precedence ladder
pub fn parse_expr_lowest_precedence(style: ExprStyle, input: ParserInput) -> ParserResult<AstNode> {
  map(
    context(
      "expression",
      tuple((|i| parse_expr_prec13(style, i), opt(parse_parenthesized_list_of_exprs))),
    ),
    |(expr, maybe_args): (AstNode, Option<Vec<AstNode>>)| -> AstNode {
      if let Some(args) = maybe_args {
        let target = CallableTarget::new_expr(expr);
        AstNodeImpl::new_application(SourceLoc::new(&input), target, args)
      } else {
        expr
      }
    },
  )(input.clone())
}
