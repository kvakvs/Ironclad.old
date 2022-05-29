//! Parse expressions and guard expressions (with added ;, operators)

use ::function_name::named;
use std::sync::Arc;

use crate::erl_syntax::erl_ast::ErlAst;
use crate::erl_syntax::erl_ast::ErlAstType::Var;
use crate::erl_syntax::node::erl_binop::ErlBinaryOperatorExpr;
use crate::erl_syntax::node::erl_callable_target::CallableTarget;
use crate::erl_syntax::node::erl_unop::ErlUnaryOperatorExpr;
use crate::erl_syntax::node::erl_var::ErlVar;
use crate::erl_syntax::parsers::misc::{
  comma, par_close, par_open, parse_varname, ws_before, ws_before_mut,
};
use crate::erl_syntax::parsers::parse_binary::BinaryParser;
use crate::erl_syntax::parsers::{AstParserResult, ErlParser, ErlParserError, VecAstParserResult};
use libironclad_error::source_loc::SourceLoc;
use nom::branch::alt;
use nom::combinator::{cut, map, opt};
use nom::multi::{many0, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, separated_pair, tuple};
use nom::{bytes, character::complete::char, error::context};

impl ErlParser {
  /// Full expression including comma operator, for function bodies
  pub const EXPR_STYLE_FULL: usize = 0;

  /// Full expression, also using comma and semicolon as boolean combinators: used in guard exprs
  pub const EXPR_STYLE_GUARD: usize = 1;

  /// Match expression: comma, semicolon not allowed, function calls not allowed, etc...
  pub const EXPR_STYLE_MATCHEXPR: usize = 2;

  /// Parse a function call (application of args to a callable value)
  #[allow(dead_code)]
  fn parse_apply(input: &str) -> AstParserResult {
    // Application consists of a callable expression, "(", list of args, and ")"
    map(
      tuple((
        Self::parse_expr,
        Self::parse_parenthesized_list_of_exprs::<{ Self::EXPR_STYLE_FULL }>,
      )),
      |(expr, args)| {
        let target = CallableTarget::new_expr(expr);
        ErlAst::new_application(SourceLoc::None, target, args)
      },
    )(input)
  }

  fn parse_var(input: &str) -> AstParserResult {
    map(parse_varname, |n| ErlAst::construct_without_location(Var(ErlVar::new(&n))))(input)
  }

  /// Parses a list of comma separated expressions in (parentheses)
  pub fn parse_parenthesized_list_of_exprs<const STYLE: usize>(
    input: &str,
  ) -> nom::IResult<&str, Vec<Arc<ErlAst>>, ErlParserError> {
    delimited(
      par_open,
      context("function application arguments", cut(Self::parse_comma_sep_exprs0::<STYLE>)),
      par_close,
    )(input)
  }

  fn parse_list_of_exprs<const STYLE: usize>(input: &str) -> AstParserResult {
    map(
      tuple((
        ws_before(char('[')),
        Self::parse_comma_sep_exprs0::<STYLE>,
        opt(preceded(ws_before(char('|')), Self::parse_expr_prec13::<STYLE>)),
        ws_before(char(']')),
      )),
      |(_open, elements, maybe_tail, _close)| {
        ErlAst::new_list(SourceLoc::None, elements, maybe_tail)
      },
    )(input)
  }

  /// Parses a `Expr <- Expr` generator
  pub fn parse_list_comprehension_generator(input: &str) -> AstParserResult {
    map(
      separated_pair(Self::parse_expr, ws_before(bytes::complete::tag("<-")), Self::parse_expr),
      |(a, b)| ErlAst::new_list_comprehension_generator(SourceLoc::None, a, b),
    )(input)
  }

  /// Parses mix of generators and conditions for a list comprehension
  pub fn parse_list_comprehension_exprs_and_generators(input: &str) -> VecAstParserResult {
    separated_list0(
      comma,
      // descend into precedence 11 instead of parse_expr, to ignore comma and semicolon
      alt((Self::parse_expr, Self::parse_list_comprehension_generator)),
    )(input)
  }

  fn parse_list_comprehension_1(input: &str) -> AstParserResult {
    map(
      separated_pair(
        Self::parse_expr,
        ws_before(bytes::complete::tag("||")),
        context(
          "list comprehension generators",
          cut(Self::parse_list_comprehension_exprs_and_generators),
        ),
      ),
      |(expr, generators)| ErlAst::new_list_comprehension(SourceLoc::None, expr, generators),
    )(input)
  }

  fn parse_list_comprehension(input: &str) -> AstParserResult {
    delimited(ws_before(char('[')), Self::parse_list_comprehension_1, ws_before(char(']')))(input)
  }

  fn parse_tuple_of_exprs<const STYLE: usize>(input: &str) -> AstParserResult {
    map(
      delimited(
        ws_before(char('{')),
        Self::parse_comma_sep_exprs0::<STYLE>,
        ws_before(char('}')),
      ),
      |elements| ErlAst::new_tuple(SourceLoc::None, elements),
    )(input)
  }

  /// Parses comma separated sequence of expressions
  pub fn parse_comma_sep_exprs0<const STYLE: usize>(
    input: &str,
  ) -> nom::IResult<&str, Vec<Arc<ErlAst>>, ErlParserError> {
    separated_list0(
      comma,
      // descend into precedence 11 instead of parse_expr, to ignore comma and semicolon
      Self::parse_expr_prec13::<STYLE>,
    )(input)
  }

  /// Parses comma separated sequence of expressions, at least one or more
  pub fn parse_comma_sep_exprs1<const STYLE: usize>(
    input: &str,
  ) -> nom::IResult<&str, Vec<Arc<ErlAst>>, ErlParserError> {
    separated_list1(
      comma,
      // descend into precedence 11 instead of parse_expr, to ignore comma and semicolon
      Self::parse_expr_prec13::<STYLE>,
    )(input)
  }

  fn parenthesized_expr<const STYLE: usize>(input: &str) -> AstParserResult {
    delimited(par_open, ws_before(Self::parse_expr_prec13::<STYLE>), par_close)(input)
  }

  /// Priority 0: (Parenthesized expressions), numbers, variables, negation (unary ops)
  fn parse_expr_prec_primary<const STYLE: usize>(input: &str) -> AstParserResult {
    match STYLE {
      Self::EXPR_STYLE_FULL => context(
        "parse expression (highest precedence)",
        ws_before_mut(alt((
          Self::parse_lambda,
          Self::parse_try_catch,
          Self::parse_if_statement,
          Self::parse_case_statement,
          BinaryParser::parse,
          Self::parenthesized_expr::<STYLE>,
          Self::parse_list_comprehension,
          Self::parse_list_of_exprs::<STYLE>,
          Self::parse_tuple_of_exprs::<STYLE>,
          Self::parse_var,
          Self::parse_literal,
        ))),
      )(input),
      Self::EXPR_STYLE_MATCHEXPR => context(
        "parse match expression (highest precedence)",
        ws_before_mut(alt((
          Self::parenthesized_expr::<STYLE>,
          Self::parse_list_of_exprs::<STYLE>,
          Self::parse_tuple_of_exprs::<STYLE>,
          Self::parse_var,
          Self::parse_literal,
        ))),
      )(input),
      Self::EXPR_STYLE_GUARD => context(
        "parse guard expression (highest precedence)",
        ws_before_mut(alt((
          Self::parenthesized_expr::<STYLE>,
          Self::parse_var,
          Self::parse_literal,
        ))),
      )(input),
      _ => panic!("STYLE={} not implemented in parse_expr", STYLE),
    }
  }

  // TODO: Precedence 1: : (colon operator, for bit fields and module access?)
  // TODO: module:function notation and maybe tuple notation?
  /// Parse expr followed by a parentheses with 0 or more args, to become a function call
  fn parse_expr_prec01<const STYLE: usize>(input: &str) -> AstParserResult {
    if STYLE == Self::EXPR_STYLE_MATCHEXPR {
      // Match expressions cannot contain module:function() style calls
      return Self::parse_expr_prec_primary::<STYLE>(input);
    }

    map(
      tuple((
        Self::parse_expr_prec_primary::<STYLE>,
        // An optional second expression after a ':', MUST be followed by parentheses with args
        opt(
          // A pair: optional second expr for function name, and mandatory args
          pair(
            opt(preceded(ws_before(char(':')), Self::parse_expr_prec_primary::<STYLE>)),
            Self::parse_parenthesized_list_of_exprs::<STYLE>,
          ),
        ),
      )),
      |(expr1, maybe_expr2_args)| {
        if let Some((maybe_expr2, args)) = maybe_expr2_args {
          match maybe_expr2 {
            Some(expr2) => {
              // TODO: merge match clause 2 and 3 as new_mfa_expr should be doing job of both?
              let target = CallableTarget::new_mfa_expr(Some(expr1), expr2, args.len());
              ErlAst::new_application(SourceLoc::None, target, args)
            }
            None => {
              let target = CallableTarget::new_expr(expr1);
              ErlAst::new_application(SourceLoc::None, target, args)
            }
          }
        } else {
          expr1 // no extra args after the expression
        }
      },
    )(input)
  }

  // TODO: Precedence 2: # (record access operator)
  fn parse_expr_prec02<const STYLE: usize>(input: &str) -> AstParserResult {
    Self::parse_expr_prec01::<STYLE>(input)
  }

  /// Precedence 3: Unary + - bnot not
  fn parse_expr_prec03<const STYLE: usize>(input: &str) -> AstParserResult {
    map(
      pair(
        ws_before_mut(alt((
          Self::unop_negative,
          Self::unop_positive,
          Self::unop_bnot,
          Self::unop_not,
        ))),
        ws_before(Self::parse_expr_prec02::<STYLE>),
      ),
      |(unop, expr)| ErlUnaryOperatorExpr::new_ast(&SourceLoc::from_input(input), unop, expr),
    )(input)
    .or_else(|_err| Self::parse_expr_prec02::<STYLE>(input))
  }

  /// Precedence 4: / * div rem band and, left associative
  fn parse_expr_prec04<const STYLE: usize>(input: &str) -> AstParserResult {
    map(
      // Higher precedence expr, followed by 0 or more operators and higher prec exprs
      pair(
        Self::parse_expr_prec03::<STYLE>,
        many0(pair(
          ws_before_mut(alt((
            Self::binop_floatdiv,
            Self::binop_multiply,
            Self::binop_intdiv,
            Self::binop_rem,
            Self::binop_band,
            Self::binop_and,
          ))),
          ws_before(Self::parse_expr_prec03::<STYLE>),
        )),
      ),
      |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }

  /// Precedence 5: + - bor bxor bsl bsr or xor, left associative
  fn parse_expr_prec05<const STYLE: usize>(input: &str) -> AstParserResult {
    map(
      // Higher precedence expr, followed by 0 or more operators and higher prec exprs
      tuple((
        Self::parse_expr_prec04::<STYLE>,
        many0(pair(
          ws_before_mut(alt((
            Self::binop_add,
            Self::binop_bor,
            Self::binop_bsl,
            Self::binop_bsr,
            Self::binop_bxor,
            Self::binop_or,
            Self::binop_subtract,
            Self::binop_xor,
          ))),
          ws_before(Self::parse_expr_prec04::<STYLE>),
        )),
      )),
      |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }

  /// Precedence 6: ++ --, right associative
  fn parse_expr_prec06<const STYLE: usize>(input: &str) -> AstParserResult {
    map(
      // Higher precedence expr, followed by 0 or more operators and higher prec exprs
      pair(
        Self::parse_expr_prec05::<STYLE>,
        many0(pair(
          ws_before_mut(alt((Self::binop_list_append, Self::binop_list_subtract))),
          ws_before(Self::parse_expr_prec05::<STYLE>),
        )),
      ),
      |(left, tail)| ErlBinaryOperatorExpr::new_right_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }

  /// Precedence 7: == /= =< < >= > =:= =/=
  fn parse_expr_prec07<const STYLE: usize>(input: &str) -> AstParserResult {
    map(
      // Higher precedence expr, followed by 0 or more operators and higher prec exprs
      pair(
        Self::parse_expr_prec06::<STYLE>,
        many0(pair(
          ws_before_mut(alt((
            Self::binop_hard_equals,
            Self::binop_hard_not_equals,
            Self::binop_not_equals,
            Self::binop_equals,
            Self::binop_less_eq,
            Self::binop_less,
            Self::binop_greater_eq,
            Self::binop_greater,
          ))),
          ws_before(Self::parse_expr_prec06::<STYLE>),
        )),
      ),
      |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }

  /// Precedence 8: andalso
  fn parse_expr_prec08<const STYLE: usize>(input: &str) -> AstParserResult {
    map(
      // Higher precedence expr, followed by 0 or more ANDALSO operators and higher prec exprs
      pair(
        Self::parse_expr_prec07::<STYLE>,
        many0(pair(
          ws_before_mut(Self::binop_andalso),
          ws_before(Self::parse_expr_prec07::<STYLE>),
        )),
      ),
      |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }

  /// Precedence 9: orelse
  fn parse_expr_prec09<const STYLE: usize>(input: &str) -> AstParserResult {
    map(
      // Higher precedence expr, followed by 0 or more ORELSE operators and higher prec exprs
      pair(
        Self::parse_expr_prec08::<STYLE>,
        many0(pair(
          ws_before_mut(Self::binop_orelse),
          ws_before(Self::parse_expr_prec08::<STYLE>),
        )),
      ),
      |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }

  /// Precedence 10: assignment/match = operator, and send operator "!", right associative
  fn parse_expr_prec10<const STYLE: usize>(input: &str) -> AstParserResult {
    map(
      // Higher precedence expr, followed by 0 or more ironclad_exe operators and higher prec exprs
      pair(
        Self::parse_expr_prec09::<STYLE>,
        many0(pair(
          ws_before_mut(alt((Self::binop_match, Self::binop_bang))),
          ws_before(Self::parse_expr_prec09::<STYLE>),
        )),
      ),
      |(left, tail)| ErlBinaryOperatorExpr::new_right_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }

  /// Precedence 11: Catch operator, then continue to higher precedences
  /// This is also entry point to parse expression when you don't want to recognize comma and semicolon
  fn parse_expr_prec11<const STYLE: usize>(input: &str) -> AstParserResult {
    // Try parse (catch Expr) otherwise try next precedence level
    map(
      pair(ws_before_mut(Self::unop_catch), ws_before(Self::parse_expr_prec10::<STYLE>)),
      |(catch_op, expr)| {
        ErlUnaryOperatorExpr::new_ast(&SourceLoc::from_input(input), catch_op, expr)
      },
    )(input)
    .or_else(|_err| ws_before(Self::parse_expr_prec10::<STYLE>)(input))
  }

  // /// Public entry point to parse expression that cannot include comma or semicolon
  // #[inline]
  // pub fn parse_expr_no_comma_no_semi(input: &str) -> AstParserResult {
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

  /// Lowest precedence 13, where we handle comma and semicolon as ironclad_exe ops.
  /// Note that semicolon is not valid for regular code only allowed in guards.
  #[named]
  fn parse_expr_prec13<const STYLE: usize>(input: &str) -> AstParserResult {
    match STYLE {
      Self::EXPR_STYLE_MATCHEXPR | Self::EXPR_STYLE_FULL =>
      // Skip comma and semicolon operator
      {
        Self::parse_expr_prec11::<STYLE>(input)
      }

      Self::EXPR_STYLE_GUARD =>
      // Guard-style expressions allow both comma and semicolons
      {
        map(
          // Higher precedence expr, followed by 0 or more ironclad_exe operators and higher prec exprs
          pair(
            ws_before(Self::parse_expr_prec11::<STYLE>),
            many0(pair(
              ws_before_mut(alt((Self::binop_semicolon, Self::binop_comma))),
              ws_before(Self::parse_expr_prec11::<STYLE>),
            )),
          ),
          |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
        )(input)
      }

      _ => unimplemented!("STYLE={} is not implemented in {}", STYLE, function_name!()),
    }
  }

  /// Parse an expression. Expression can also be a block which produces a value.
  pub fn parse_expr(input: &str) -> AstParserResult {
    context("expression", Self::parse_expr_prec13::<{ Self::EXPR_STYLE_FULL }>)(input)
  }

  /// Parse a guard expression.
  pub fn parse_guardexpr(input: &str) -> AstParserResult {
    context("guard expression", Self::parse_expr_prec13::<{ Self::EXPR_STYLE_GUARD }>)(input)
  }

  /// Parse a match-expression. Match-expression cannot be a block or a function call, no comma and semicolon.
  pub fn parse_matchexpr(input: &str) -> AstParserResult {
    context("match expression", Self::parse_expr_prec13::<{ Self::EXPR_STYLE_MATCHEXPR }>)(input)
  }
}
