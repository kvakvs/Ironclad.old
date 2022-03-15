//! Parse expressions and guard expressions (with added ;, operators)

use ::function_name::named;
use std::sync::Arc;

use nom::{bytes, character::complete::{char}, error::{context}, combinator, sequence, multi, branch};

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::node::erl_binop::{ErlBinaryOperatorExpr};
use crate::erlang::syntax_tree::node::erl_callable_target::CallableTarget;
use crate::erlang::syntax_tree::node::erl_unop::ErlUnaryOperatorExpr;
use crate::erlang::syntax_tree::node::erl_var::ErlVar;
use crate::erlang::syntax_tree::nom_parse::{AstParserResult, ErlParser, ErlParserError, VecAstParserResult};
use crate::source_loc::SourceLoc;


impl ErlParser {
  /// Full expression including comma operator, for function bodies
  pub const EXPR_STYLE_FULL: usize = 0;

  /// Full expression, also using comma and semicolon as boolean combinators: used in guard exprs
  pub const EXPR_STYLE_GUARD: usize = 1;

  /// Match expression: comma, semicolon not allowed, function calls not allowed, etc...
  pub const EXPR_STYLE_MATCHEXPR: usize = 2;

  /// Parse a function call (application of args to a callable value)
  fn parse_apply(input: &str) -> AstParserResult {
    // Application consists of a callable expression, "(", list of args, and ")"
    combinator::map(
      sequence::tuple((
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
    let (input, name) = Self::parse_ident_capitalized(input)?;
    let ast = ErlAst::Var(ErlVar::new(SourceLoc::None, &name)).into();
    Ok((input, ast))
  }

  /// Parses a list of comma separated expressions in (parentheses)
  pub fn parse_parenthesized_list_of_exprs<const STYLE: usize>(
    input: &str
  ) -> nom::IResult<&str, Vec<Arc<ErlAst>>, ErlParserError> {
    sequence::delimited(
      Self::ws_before(char('(')),
      Self::parse_comma_sep_exprs0::<STYLE>,
      Self::ws_before(char(')')),
    )(input)
  }

  fn parse_list_of_exprs<const STYLE: usize>(input: &str) -> AstParserResult {
    combinator::map(
      sequence::tuple((
        Self::ws_before(char('[')),
        Self::parse_comma_sep_exprs0::<STYLE>,
        combinator::opt(
          sequence::preceded(
            Self::ws_before(char('|')),
            Self::parse_expr_prec13::<STYLE>,
          )
        ),
        Self::ws_before(char(']')),
      )),
      |(_open, elements, maybe_tail, _close)| {
        ErlAst::new_list(SourceLoc::None, elements, maybe_tail)
      },
    )(input)
  }

  /// Parses a `Expr <- Expr` generator
  pub fn parse_list_comprehension_generator(input: &str) -> AstParserResult {
    combinator::map(
      sequence::separated_pair(
        Self::parse_expr,
        Self::ws_before(bytes::complete::tag("<-")),
        Self::parse_expr,
      ),
      |(a, b)| ErlAst::new_list_comprehension_generator(SourceLoc::None, a, b),
    )(input)
  }

  /// Parses mix of generators and conditions for a list comprehension
  pub fn parse_list_comprehension_exprs_and_generators(input: &str) -> VecAstParserResult {
    multi::separated_list0(
      Self::ws_before(char(',')),
      // descend into precedence 11 instead of parse_expr, to ignore comma and semicolon
      branch::alt((
        Self::parse_expr,
        Self::parse_list_comprehension_generator
      )),
    )(input)
  }

  fn parse_list_comprehension_1(input: &str) -> AstParserResult {
    combinator::map(
      sequence::separated_pair(
        Self::parse_expr,
        Self::ws_before(bytes::complete::tag("||")),
        Self::parse_list_comprehension_exprs_and_generators,
      ),
      |(expr, generators)| {
        ErlAst::new_list_comprehension(SourceLoc::None, expr, generators)
      },
    )(input)
  }

  fn parse_list_comprehension(input: &str) -> AstParserResult {
    sequence::delimited(
      Self::ws_before(char('[')),
      Self::parse_list_comprehension_1,
      Self::ws_before(char(']')),
    )(input)
  }

  fn parse_tuple_of_exprs<const STYLE: usize>(input: &str) -> AstParserResult {
    combinator::map(
      sequence::delimited(
        Self::ws_before(char('{')),
        Self::parse_comma_sep_exprs0::<STYLE>,
        Self::ws_before(char('}')),
      ),
      |elements| ErlAst::new_tuple(SourceLoc::None, elements),
    )(input)
  }

  /// Parses comma separated sequence of expressions
  pub fn parse_comma_sep_exprs0<const STYLE: usize>(
    input: &str
  ) -> nom::IResult<&str, Vec<Arc<ErlAst>>, ErlParserError> {
    multi::separated_list0(
      Self::ws_before(char(',')),
      // descend into precedence 11 instead of parse_expr, to ignore comma and semicolon
      Self::parse_expr_prec13::<STYLE>,
    )(input)
  }

  /// Parses comma separated sequence of expressions, at least one or more
  pub fn parse_comma_sep_exprs1<const STYLE: usize>(
    input: &str
  ) -> nom::IResult<&str, Vec<Arc<ErlAst>>, ErlParserError> {
    multi::separated_list1(
      Self::ws_before(char(',')),
      // descend into precedence 11 instead of parse_expr, to ignore comma and semicolon
      Self::parse_expr_prec13::<STYLE>,
    )(input)
  }

  fn parenthesized_expr<const STYLE: usize>(input: &str) -> AstParserResult {
    sequence::delimited(
      Self::ws_before(char('(')),
      Self::ws_before(Self::parse_expr_prec13::<STYLE>),
      Self::ws_before(char(')')),
    )(input)
  }

  /// Priority 0: (Parenthesized expressions), numbers, variables, negation (unary ops)
  fn parse_expr_prec_primary<const STYLE: usize>(input: &str) -> AstParserResult {
    match STYLE {
      Self::EXPR_STYLE_FULL =>
        context("parse expression (primary precedence)",
                Self::ws_before_mut(
                  branch::alt((
                    Self::parse_try_catch,
                    Self::parse_if_statement,
                    Self::parenthesized_expr::<STYLE>,
                    Self::parse_list_comprehension,
                    Self::parse_list_of_exprs::<STYLE>,
                    Self::parse_tuple_of_exprs::<STYLE>,
                    Self::parse_var,
                    Self::parse_literal,
                  ))
                ))(input),
      Self::EXPR_STYLE_MATCHEXPR =>
        context("parse match expression (primary precedence)",
                Self::ws_before_mut(
                  branch::alt((
                    Self::parenthesized_expr::<STYLE>,
                    Self::parse_list_of_exprs::<STYLE>,
                    Self::parse_tuple_of_exprs::<STYLE>,
                    Self::parse_var,
                    Self::parse_literal,
                  ))
                ))(input),
      Self::EXPR_STYLE_GUARD =>
        context("parse guard expression (primary precedence)",
                Self::ws_before_mut(
                  branch::alt((
                    Self::parenthesized_expr::<STYLE>,
                    Self::parse_var,
                    Self::parse_literal,
                  ))
                ))(input),
      _ => panic!("STYLE={} not implemented in parse_expr", STYLE)
    }
  }

  // TODO: Precedence 1: : (colon operator, for bit fields and module access?)
  // TODO: module:function notation and maybe tuple notation?
  /// Parse expr followed by a parentheses with 0 or more args, to become a function call
  fn parse_expr_prec01<const STYLE: usize>(input: &str) -> AstParserResult {
    combinator::map(
      sequence::tuple((
        Self::parse_expr_prec_primary::<STYLE>,

        // An optional second expression after a ':'
        combinator::opt(sequence::preceded(
          char(':'),
          Self::parse_expr_prec_primary::<STYLE>)),
        combinator::opt(Self::parse_parenthesized_list_of_exprs::<STYLE>),
      )),
      |(expr1, maybe_expr2, maybe_args)| {
        match (maybe_expr2, maybe_args) {
          (_, None) => expr1,
          (Some(expr2), Some(args_vec)) => {
            // TODO: merge match clause 2 and 3 as new_mfa_expr should be doing job of both?
            let target = CallableTarget::new_mfa_expr(Some(expr1), expr2, args_vec.len());
            ErlAst::new_application(SourceLoc::None, target, args_vec)
          }
          (None, Some(args_vec)) => {
            let target = CallableTarget::new_expr(expr1);
            ErlAst::new_application(SourceLoc::None, target, args_vec)
          }
        }
      })(input)
  }

  // TODO: Precedence 2: # (record access operator)
  fn parse_expr_prec02<const STYLE: usize>(input: &str) -> AstParserResult {
    Self::parse_expr_prec01::<STYLE>(input)
  }

  /// Precedence 3: Unary + - bnot not
  fn parse_expr_prec03<const STYLE: usize>(input: &str) -> AstParserResult {
    combinator::map(
      sequence::pair(
        Self::ws_before_mut(branch::alt((
          Self::unop_negative, Self::unop_positive,
          Self::unop_bnot, Self::unop_not,
        ))),
        Self::ws_before(Self::parse_expr_prec02::<STYLE>),
      ),
      |(unop, expr)| ErlUnaryOperatorExpr::new_ast(SourceLoc::None, unop, expr),
    )(input)
        .or_else(|_err| Self::parse_expr_prec02::<STYLE>(input))
  }

  /// Precedence 4: / * div rem band and, left associative
  fn parse_expr_prec04<const STYLE: usize>(input: &str) -> AstParserResult {
    combinator::map(
      // Higher precedence expr, followed by 0 or more operators and higher prec exprs
      sequence::pair(
        Self::parse_expr_prec03::<STYLE>,
        multi::many0(
          sequence::pair(
            Self::ws_before_mut(branch::alt((
              Self::binop_floatdiv, Self::binop_multiply,
              Self::binop_intdiv, Self::binop_rem,
              Self::binop_band, Self::binop_and,
            ))),
            Self::ws_before(Self::parse_expr_prec03::<STYLE>),
          )
        ),
      ),
      |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }

  /// Precedence 5: + - bor bxor bsl bsr or xor, left associative
  fn parse_expr_prec05<const STYLE: usize>(input: &str) -> AstParserResult {
    combinator::map(
      // Higher precedence expr, followed by 0 or more operators and higher prec exprs
      sequence::tuple((
        Self::parse_expr_prec04::<STYLE>,
        multi::many0(
          sequence::pair(
            Self::ws_before_mut(branch::alt((
              Self::binop_add,
              Self::binop_bor,
              Self::binop_bsl,
              Self::binop_bsr,
              Self::binop_bxor,
              Self::binop_or,
              Self::binop_subtract,
              Self::binop_xor,
            ))),
            Self::ws_before(Self::parse_expr_prec04::<STYLE>),
          )
        )
      )),
      |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }

  /// Precedence 6: ++ --, right associative
  fn parse_expr_prec06<const STYLE: usize>(input: &str) -> AstParserResult {
    combinator::map(
      // Higher precedence expr, followed by 0 or more operators and higher prec exprs
      sequence::pair(
        Self::parse_expr_prec05::<STYLE>,
        multi::many0(
          sequence::pair(
            Self::ws_before_mut(branch::alt((
              Self::binop_list_append,
              Self::binop_list_subtract,
            ))),
            Self::ws_before(Self::parse_expr_prec05::<STYLE>),
          )
        ),
      ),
      |(left, tail)| ErlBinaryOperatorExpr::new_right_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }

  /// Precedence 7: == /= =< < >= > =:= =/=
  fn parse_expr_prec07<const STYLE: usize>(input: &str) -> AstParserResult {
    combinator::map(
      // Higher precedence expr, followed by 0 or more operators and higher prec exprs
      sequence::pair(
        Self::parse_expr_prec06::<STYLE>,
        multi::many0(
          sequence::pair(
            Self::ws_before_mut(branch::alt((
              Self::binop_hard_equals,
              Self::binop_hard_not_equals,
              Self::binop_not_equals,
              Self::binop_equals,
              Self::binop_less_eq,
              Self::binop_less,
              Self::binop_greater_eq,
              Self::binop_greater,
            ))),
            Self::ws_before(Self::parse_expr_prec06::<STYLE>),
          )
        ),
      ),
      |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }

  /// Precedence 8: andalso
  fn parse_expr_prec08<const STYLE: usize>(input: &str) -> AstParserResult {
    combinator::map(
      // Higher precedence expr, followed by 0 or more ANDALSO operators and higher prec exprs
      sequence::pair(
        Self::parse_expr_prec07::<STYLE>,
        multi::many0(
          sequence::pair(
            Self::ws_before_mut(Self::binop_andalso),
            Self::ws_before(Self::parse_expr_prec07::<STYLE>),
          )
        ),
      ),
      |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }

  /// Precedence 9: orelse
  fn parse_expr_prec09<const STYLE: usize>(input: &str) -> AstParserResult {
    combinator::map(
      // Higher precedence expr, followed by 0 or more ORELSE operators and higher prec exprs
      sequence::pair(
        Self::parse_expr_prec08::<STYLE>,
        multi::many0(
          sequence::pair(
            Self::ws_before_mut(Self::binop_orelse),
            Self::ws_before(Self::parse_expr_prec08::<STYLE>),
          )
        ),
      ),
      |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }

  /// Precedence 10: assignment/match = operator, and send operator "!", right associative
  fn parse_expr_prec10<const STYLE: usize>(input: &str) -> AstParserResult {
    combinator::map(
      // Higher precedence expr, followed by 0 or more binary operators and higher prec exprs
      sequence::pair(
        Self::parse_expr_prec09::<STYLE>,
        multi::many0(
          sequence::pair(
            Self::ws_before_mut(branch::alt((
              Self::binop_match, Self::binop_bang
            ))),
            Self::ws_before(Self::parse_expr_prec09::<STYLE>),
          )
        ),
      ),
      |(left, tail)| ErlBinaryOperatorExpr::new_right_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }

  /// Precedence 11: Catch operator, then continue to higher precedences
  /// This is also entry point to parse expression when you don't want to recognize comma and semicolon
  fn parse_expr_prec11<const STYLE: usize>(input: &str) -> AstParserResult {
    // Try parse (catch Expr) otherwise try next precedence level
    combinator::map(
      sequence::pair(
        Self::ws_before_mut(Self::unop_catch),
        Self::ws_before(Self::parse_expr_prec10::<STYLE>),
      ),
      |(catch_op, expr)| ErlUnaryOperatorExpr::new_ast(SourceLoc::None, catch_op, expr),
    )(input)
        .or_else(|_err| Self::ws_before(Self::parse_expr_prec10::<STYLE>)(input))
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

  /// Lowest precedence 13, where we handle comma and semicolon as binary ops.
  /// Note that semicolon is not valid for regular code only allowed in guards.
  #[named]
  fn parse_expr_prec13<const STYLE: usize>(input: &str) -> AstParserResult {
    match STYLE {
      Self::EXPR_STYLE_MATCHEXPR | Self::EXPR_STYLE_FULL =>
      // Skip comma and semicolon operator
        return Self::parse_expr_prec11::<STYLE>(input),

      Self::EXPR_STYLE_GUARD =>
      // Guard-style expressions allow both comma and semicolons
        combinator::map(
          // Higher precedence expr, followed by 0 or more binary operators and higher prec exprs
          sequence::pair(
            Self::ws_before(Self::parse_expr_prec11::<STYLE>),
            multi::many0(
              sequence::pair(
                Self::ws_before_mut(branch::alt((
                  Self::binop_semicolon, Self::binop_comma
                ))),
                Self::ws_before(Self::parse_expr_prec11::<STYLE>),
              )
            ),
          ),
          |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
        )(input),

      _ => unimplemented!("STYLE={} is not implemented in {}", STYLE, function_name!())
    }
  }

  /// Parse an expression. Expression can also be a block which produces a value.
  pub fn parse_expr(input: &str) -> AstParserResult {
    context("expression",
            Self::parse_expr_prec13::<{ Self::EXPR_STYLE_FULL }>)(input)
  }

  /// Parse a guard expression.
  pub fn parse_guardexpr(input: &str) -> AstParserResult {
    context("guard expression",
            Self::parse_expr_prec13::<{ Self::EXPR_STYLE_GUARD }>)(input)
  }

  /// Parse a match-expression. Match-expression cannot be a block or a function call, no comma and semicolon.
  pub fn parse_matchexpr(input: &str) -> AstParserResult {
    context("match expression",
            Self::parse_expr_prec13::<{ Self::EXPR_STYLE_MATCHEXPR }>)(input)
  }
}