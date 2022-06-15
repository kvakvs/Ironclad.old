//! Parse expressions and guard expressions (with added ;, operators)

use ::function_name::named;

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::node_impl::AstNodeType::Var;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_binop::ErlBinaryOperatorExpr;
use crate::erl_syntax::node::erl_callable_target::CallableTarget;
use crate::erl_syntax::node::erl_map::MapBuilderMember;
use crate::erl_syntax::node::erl_unop::ErlUnaryOperatorExpr;
use crate::erl_syntax::node::erl_var::ErlVar;
use crate::erl_syntax::parsers::defs::{ErlParserError, ParserResult, VecAstParserResult};
use crate::erl_syntax::parsers::misc::{tok, tok_var};
use crate::erl_syntax::parsers::parse_binary::parse_binary;
use crate::erl_syntax::parsers::parse_case::parse_case_statement;
use crate::erl_syntax::parsers::parse_expr_op::{
  binop_add, binop_and, binop_andalso, binop_band, binop_bang, binop_bor, binop_bsl, binop_bsr,
  binop_bxor, binop_comma, binop_equals, binop_floatdiv, binop_greater, binop_greater_eq,
  binop_hard_equals, binop_hard_not_equals, binop_intdiv, binop_less, binop_less_eq,
  binop_list_append, binop_list_subtract, binop_match, binop_multiply, binop_not_equals, binop_or,
  binop_orelse, binop_rem, binop_semicolon, binop_subtract, binop_xor, unop_bnot, unop_catch,
  unop_negative, unop_not, unop_positive,
};
use crate::erl_syntax::parsers::parse_fn::parse_lambda;
use crate::erl_syntax::parsers::parse_if_stmt::parse_if_statement;
use crate::erl_syntax::parsers::parse_lit::parse_erl_literal;
use crate::erl_syntax::parsers::parse_try_catch::parse_try_catch;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::source_loc::SourceLoc;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{cut, map, opt};
use nom::multi::{many0, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, separated_pair, tuple};
use nom::{character::complete::char, error::context};

/// Full expression including comma operator, for function bodies
pub const EXPR_STYLE_FULL: usize = 0;

/// Full expression, also using comma and semicolon as boolean combinators: used in guard exprs
pub const EXPR_STYLE_GUARD: usize = 1;

/// Match expression: comma, semicolon not allowed, function calls not allowed, etc...
pub const EXPR_STYLE_MATCHEXPR: usize = 2;

/// Parse a function call (application of args to a callable value)
#[allow(dead_code)]
fn parse_apply(input: ParserInput) -> ParserResult<AstNode> {
  // Application consists of a callable expression, "(", list of args, and ")"
  map(
    tuple((parse_expr, parse_parenthesized_list_of_exprs::<{ EXPR_STYLE_FULL }>)),
    |(expr, args)| {
      let target = CallableTarget::new_expr(expr);
      AstNodeImpl::new_application(SourceLoc::new(input), target, args)
    },
  )(input.clone())
}

fn parse_var(input: ParserInput) -> ParserResult<AstNode> {
  map(tok_var, |n| AstNodeImpl::construct_without_location(Var(ErlVar::new(&n))))(input)
}

/// Parses a list of comma separated expressions in (parentheses)
pub fn parse_parenthesized_list_of_exprs<const STYLE: usize>(
  input: ParserInput,
) -> nom::IResult<ParserInput, Vec<AstNode>, ErlParserError> {
  delimited(
    tok(TokenType::ParOpen),
    context("function application arguments", cut(parse_comma_sep_exprs0::<STYLE>)),
    tok(TokenType::ParClose),
  )(input)
}

fn parse_list_of_exprs<const STYLE: usize>(input: ParserInput) -> ParserResult<AstNode> {
  map(
    tuple((
      tok(TokenType::SquareOpen),
      parse_comma_sep_exprs0::<STYLE>,
      opt(preceded(tok(TokenType::Bar), parse_expr_prec13::<STYLE>)),
      tok(TokenType::SquareClose),
    )),
    |(_open, elements, maybe_tail, _close)| {
      AstNodeImpl::new_list(SourceLoc::new(input), elements, maybe_tail)
    },
  )(input.clone())
}

/// Parses a `Expr <- Expr` generator
pub fn parse_list_comprehension_generator(input: ParserInput) -> ParserResult<AstNode> {
  map(separated_pair(parse_expr, tok(TokenType::LeftArr), parse_expr), |(a, b)| {
    AstNodeImpl::new_list_comprehension_generator(SourceLoc::new(input), a, b)
  })(input.clone())
}

/// Parses mix of generators and conditions for a list comprehension
pub fn parse_list_comprehension_exprs_and_generators(input: ParserInput) -> VecAstParserResult {
  separated_list0(
    tok(TokenType::Comma),
    // descend into precedence 11 instead of parse_expr, to ignore comma and semicolon
    alt((parse_expr, parse_list_comprehension_generator)),
  )(input)
}

fn parse_list_comprehension_1(input: ParserInput) -> ParserResult<AstNode> {
  map(
    separated_pair(
      parse_expr,
      tok(TokenType::BarBar),
      context(
        "list comprehension generators",
        cut(parse_list_comprehension_exprs_and_generators),
      ),
    ),
    |(expr, generators)| {
      AstNodeImpl::new_list_comprehension(SourceLoc::new(input), expr, generators)
    },
  )(input.clone())
}

fn parse_list_comprehension(input: ParserInput) -> ParserResult<AstNode> {
  delimited(
    tok(TokenType::SquareOpen),
    parse_list_comprehension_1,
    tok(TokenType::SquareClose),
  )(input)
}

/// Parse a sequence of curly braced expressions `"{" EXPR1 "," EXPR2 "," ... "}"`
fn parse_tuple_of_exprs<const STYLE: usize>(input: ParserInput) -> ParserResult<AstNode> {
  map(
    delimited(
      tok(TokenType::CurlyOpen),
      parse_comma_sep_exprs0::<STYLE>,
      tok(TokenType::CurlyClose),
    ),
    |elements| AstNodeImpl::new_tuple(SourceLoc::new(input), elements),
  )(input.clone())
}

/// Parse one member of a map builder `keyExpr "=>" valueExpr`
fn map_builder_member<const STYLE: usize>(input: ParserInput) -> ParserResult<MapBuilderMember> {
  map(
    separated_pair(
      parse_expr_prec13::<STYLE>,
      tok(TokenType::RightDoubleArr),
      parse_expr_prec13::<STYLE>,
    ),
    |(key, value)| MapBuilderMember { key, value },
  )(input)
}

/// Parse a map builder expression, which uses `=>` to assign the values.
/// Contrary to a map matcher, which would use `:=`.
fn map_builder_of_exprs<const STYLE: usize>(input: ParserInput) -> ParserResult<AstNode> {
  map(
    delimited(
      pair(tok(TokenType::Hash), tok(TokenType::CurlyOpen)),
      separated_list0(tok(TokenType::Comma), map_builder_member::<STYLE>),
      tok(TokenType::CurlyClose),
    ),
    |members| AstNodeImpl::new_map_builder(SourceLoc::new(input), members),
  )(input.clone())
}

/// Parses comma separated sequence of expressions
pub fn parse_comma_sep_exprs0<const STYLE: usize>(
  input: ParserInput,
) -> nom::IResult<ParserInput, Vec<AstNode>, ErlParserError> {
  separated_list0(
    tok(TokenType::Comma),
    // descend into precedence 11 instead of parse_expr, to ignore comma and semicolon
    parse_expr_prec13::<STYLE>,
  )(input)
}

/// Parses comma separated sequence of expressions, at least one or more
pub fn parse_comma_sep_exprs1<const STYLE: usize>(
  input: ParserInput,
) -> nom::IResult<ParserInput, Vec<AstNode>, ErlParserError> {
  separated_list1(
    tok(TokenType::Comma),
    // descend into precedence 11 instead of parse_expr, to ignore comma and semicolon
    parse_expr_prec13::<STYLE>,
  )(input)
}

fn parenthesized_expr<const STYLE: usize>(input: ParserInput) -> ParserResult<AstNode> {
  delimited(tok(TokenType::ParOpen), parse_expr_prec13::<STYLE>, tok(TokenType::ParClose))(input)
}

/// Priority 0: (Parenthesized expressions), numbers, variables, negation (unary ops)
fn parse_expr_prec_primary<const STYLE: usize>(input: ParserInput) -> ParserResult<AstNode> {
  match STYLE {
    EXPR_STYLE_FULL => context(
      "parse expression (highest precedence)",
      alt((
        parse_lambda,
        parse_try_catch,
        parse_if_statement,
        parse_case_statement,
        parse_binary,
        parenthesized_expr::<STYLE>,
        parse_list_comprehension,
        parse_list_of_exprs::<STYLE>,
        parse_tuple_of_exprs::<STYLE>,
        map_builder_of_exprs::<STYLE>,
        parse_var,
        parse_erl_literal,
      )),
    )(input),
    EXPR_STYLE_MATCHEXPR => context(
      "parse match expression (highest precedence)",
      alt((
        parenthesized_expr::<STYLE>,
        parse_list_of_exprs::<STYLE>,
        parse_tuple_of_exprs::<STYLE>,
        parse_var,
        parse_erl_literal,
        parse_binary,
      )),
    )(input),
    EXPR_STYLE_GUARD => context(
      "parse guard expression (highest precedence)",
      alt((parenthesized_expr::<STYLE>, parse_var, parse_erl_literal)),
    )(input),
    _ => panic!("STYLE={} not implemented in parse_expr", STYLE),
  }
}

// TODO: Precedence 1: : (colon operator, for bit fields and module access?)
// TODO: module:function notation and maybe tuple notation?
/// Parse expr followed by a parentheses with 0 or more args, to become a function call
fn parse_expr_prec01<const STYLE: usize>(input: ParserInput) -> ParserResult<AstNode> {
  if STYLE == EXPR_STYLE_MATCHEXPR {
    // Match expressions cannot contain module:function() style calls
    return parse_expr_prec_primary::<STYLE>(input);
  }

  map(
    tuple((
      parse_expr_prec_primary::<STYLE>,
      // An optional second expression after a ':', MUST be followed by parentheses with args
      opt(
        // A pair: optional second expr for function name, and mandatory args
        pair(
          opt(preceded(tok(TokenType::Colon), parse_expr_prec_primary::<STYLE>)),
          parse_parenthesized_list_of_exprs::<STYLE>,
        ),
      ),
    )),
    |(expr1, maybe_expr2_args)| {
      if let Some((maybe_expr2, args)) = maybe_expr2_args {
        match maybe_expr2 {
          Some(expr2) => {
            // TODO: merge match clause 2 and 3 as new_mfa_expr should be doing job of both?
            let target = CallableTarget::new_mfa_expr(Some(expr1), expr2, args.len());
            AstNodeImpl::new_application(SourceLoc::new(input), target, args)
          }
          None => {
            let target = CallableTarget::new_expr(expr1);
            AstNodeImpl::new_application(SourceLoc::new(input), target, args)
          }
        }
      } else {
        expr1 // no extra args after the expression
      }
    },
  )(input.clone())
}

// TODO: Precedence 2: # (record access operator)
fn parse_expr_prec02<const STYLE: usize>(input: ParserInput) -> ParserResult<AstNode> {
  parse_expr_prec01::<STYLE>(input)
}

/// Precedence 3: Unary + - bnot not
fn parse_expr_prec03<const STYLE: usize>(input: ParserInput) -> ParserResult<AstNode> {
  map(
    pair(
      alt((unop_negative, unop_positive, unop_bnot, unop_not)),
      parse_expr_prec02::<STYLE>,
    ),
    |(unop, expr)| ErlUnaryOperatorExpr::new_ast(SourceLoc::new(input), unop, expr),
  )(input.clone())
  .or_else(|_err| parse_expr_prec02::<STYLE>(input.clone()))
}

/// Precedence 4: / * div rem band and, left associative
fn parse_expr_prec04<const STYLE: usize>(input: ParserInput) -> ParserResult<AstNode> {
  map(
    // Higher precedence expr, followed by 0 or more operators and higher prec exprs
    pair(
      parse_expr_prec03::<STYLE>,
      many0(pair(
        alt((binop_floatdiv, binop_multiply, binop_intdiv, binop_rem, binop_band, binop_and)),
        parse_expr_prec03::<STYLE>,
      )),
    ),
    |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(SourceLoc::None, left, &tail),
  )(input)
}

/// Precedence 5: + - bor bxor bsl bsr or xor, left associative
fn parse_expr_prec05<const STYLE: usize>(input: ParserInput) -> ParserResult<AstNode> {
  map(
    // Higher precedence expr, followed by 0 or more operators and higher prec exprs
    tuple((
      parse_expr_prec04::<STYLE>,
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
        parse_expr_prec04::<STYLE>,
      )),
    )),
    |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(SourceLoc::None, left, &tail),
  )(input)
}

/// Precedence 6: ++ --, right associative
fn parse_expr_prec06<const STYLE: usize>(input: ParserInput) -> ParserResult<AstNode> {
  map(
    // Higher precedence expr, followed by 0 or more operators and higher prec exprs
    pair(
      parse_expr_prec05::<STYLE>,
      many0(pair(alt((binop_list_append, binop_list_subtract)), parse_expr_prec05::<STYLE>)),
    ),
    |(left, tail)| ErlBinaryOperatorExpr::new_right_assoc(SourceLoc::None, left, &tail),
  )(input)
}

/// Precedence 7: == /= =< < >= > =:= =/=
fn parse_expr_prec07<const STYLE: usize>(input: ParserInput) -> ParserResult<AstNode> {
  map(
    // Higher precedence expr, followed by 0 or more operators and higher prec exprs
    pair(
      parse_expr_prec06::<STYLE>,
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
        parse_expr_prec06::<STYLE>,
      )),
    ),
    |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(SourceLoc::None, left, &tail),
  )(input)
}

/// Precedence 8: andalso
fn parse_expr_prec08<const STYLE: usize>(input: ParserInput) -> ParserResult<AstNode> {
  map(
    // Higher precedence expr, followed by 0 or more ANDALSO operators and higher prec exprs
    pair(
      parse_expr_prec07::<STYLE>,
      many0(pair(binop_andalso, parse_expr_prec07::<STYLE>)),
    ),
    |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(SourceLoc::None, left, &tail),
  )(input)
}

/// Precedence 9: orelse
fn parse_expr_prec09<const STYLE: usize>(input: ParserInput) -> ParserResult<AstNode> {
  map(
    // Higher precedence expr, followed by 0 or more ORELSE operators and higher prec exprs
    pair(
      parse_expr_prec08::<STYLE>,
      many0(pair(binop_orelse, parse_expr_prec08::<STYLE>)),
    ),
    |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(SourceLoc::None, left, &tail),
  )(input)
}

/// Precedence 10: assignment/match = operator, and send operator "!", right associative
fn parse_expr_prec10<const STYLE: usize>(input: ParserInput) -> ParserResult<AstNode> {
  map(
    // Higher precedence expr, followed by 0 or more binary operators and higher prec exprs
    pair(
      parse_expr_prec09::<STYLE>,
      many0(pair(alt((binop_match, binop_bang)), parse_expr_prec09::<STYLE>)),
    ),
    |(left, tail)| ErlBinaryOperatorExpr::new_right_assoc(SourceLoc::None, left, &tail),
  )(input)
}

/// Precedence 11: Catch operator, then continue to higher precedences
/// This is also entry point to parse expression when you don't want to recognize comma and semicolon
fn parse_expr_prec11<const STYLE: usize>(input: ParserInput) -> ParserResult<AstNode> {
  // Try parse (catch Expr) otherwise try next precedence level
  map(pair(unop_catch, parse_expr_prec10::<STYLE>), |(catch_op, expr)| {
    ErlUnaryOperatorExpr::new_ast(SourceLoc::new(input), catch_op, expr)
  })(input.clone())
  .or_else(|_err| parse_expr_prec10::<STYLE>(input.clone()))
}

// /// Public entry point to parse expression that cannot include comma or semicolon
// #[inline]
// pub fn parse_expr_no_comma_no_semi(input: ParserInput) -> ParserResult<AstNode> {
//   Self::parse_expr_prec11(input)
//   // println!("parse_expr_no_comma_no_semi: {}", input);
//   // match Self::parse_prec11(input) {
//   //   Ok((tail, result)) => {
//   //     println!("OK r={}", result.deref());
//   //     Ok((tail, result))
//   //   }
//   //   Err(e) => Err(e)
//   // }
// }

/// Lowest precedence 13, where we handle comma and semicolon as binary ops.
/// Note that semicolon is not valid for regular code only allowed in guards.
#[named]
fn parse_expr_prec13<const STYLE: usize>(input: ParserInput) -> ParserResult<AstNode> {
  match STYLE {
    EXPR_STYLE_MATCHEXPR | EXPR_STYLE_FULL =>
    // Skip comma and semicolon operator
    {
      parse_expr_prec11::<STYLE>(input)
    }

    EXPR_STYLE_GUARD =>
    // Guard-style expressions allow both comma and semicolons
    {
      map(
        // Higher precedence expr, followed by 0 or more binary operators and higher prec exprs
        pair(
          parse_expr_prec11::<STYLE>,
          many0(pair(alt((binop_semicolon, binop_comma)), parse_expr_prec11::<STYLE>)),
        ),
        |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(SourceLoc::None, left, &tail),
      )(input)
    }

    _ => unimplemented!("STYLE={} is not implemented in {}", STYLE, function_name!()),
  }
}

/// Parse an expression. Expression can also be a block which produces a value.
pub fn parse_expr(input: ParserInput) -> ParserResult<AstNode> {
  context("expression", parse_expr_prec13::<{ EXPR_STYLE_FULL }>)(input)
}

/// Parse a guard expression.
pub fn parse_guardexpr(input: ParserInput) -> ParserResult<AstNode> {
  context("guard expression", parse_expr_prec13::<{ EXPR_STYLE_GUARD }>)(input)
}

/// Parse a match-expression. Match-expression cannot be a block or a function call, no comma and semicolon.
pub fn parse_matchexpr(input: ParserInput) -> ParserResult<AstNode> {
  context("match expression", parse_expr_prec13::<{ EXPR_STYLE_MATCHEXPR }>)(input)
}
