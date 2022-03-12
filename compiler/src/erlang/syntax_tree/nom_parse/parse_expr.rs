//! Parse expressions and guard expressions (with added ;, operators)

use std::sync::Arc;

use nom::{character::complete::{char}, combinator, sequence, multi, branch};

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::node::erl_apply::ErlApply;
use crate::erlang::syntax_tree::node::erl_binop::{ErlBinaryOperatorExpr};
use crate::erlang::syntax_tree::node::erl_unop::ErlUnaryOperatorExpr;
use crate::erlang::syntax_tree::node::erl_var::ErlVar;
use crate::erlang::syntax_tree::nom_parse::{AstParserResult, ErlParser, ErlParserError};
use crate::source_loc::SourceLoc;

impl ErlParser {
  /// Parse a function call (application of args to a callable value)
  fn parse_apply(input: &str) -> AstParserResult {
    // Application consists of a callable expression, "(", list of args, and ")"
    combinator::map(
      sequence::tuple((
        Self::parse_expr,
        Self::parse_parenthesized_list_of_exprs,
        // sequence::delimited(
        //   Self::ws_before(char('(')),
        //   multi::separated_list0(
        //     Self::ws_before(char(',')),
        //     Self::parse_expr_no_comma_no_semi),
        //   Self::ws_before(char(')')),
        // ),
      )),
      |(expr, args)| {
        let application = ErlApply::new(SourceLoc::None, expr, args);
        ErlAst::Apply(application).into()
      },
    )(input)
  }

  fn parse_var(input: &str) -> AstParserResult {
    let (input, name) = Self::parse_ident_capitalized(input)?;
    let ast = ErlAst::Var(ErlVar::new(SourceLoc::None, &name)).into();
    Ok((input, ast))
  }

  /// Parses a list of comma separated expressions in (parentheses)
  pub fn parse_parenthesized_list_of_exprs(input: &str) -> nom::IResult<&str, Vec<Arc<ErlAst>>, ErlParserError> {
    sequence::delimited(
      Self::ws_before(char('(')),
      Self::parse_comma_sep_exprs,
      Self::ws_before(char(')')),
    )(input)
  }

  fn parse_list_of_exprs(input: &str) -> AstParserResult {
    // TODO: list tail with |
    combinator::map(
      sequence::delimited(
        Self::ws_before(char('[')),
        Self::parse_comma_sep_exprs,
        Self::ws_before(char(']')),
      ),
      |elements| ErlAst::new_list(SourceLoc::None, elements),
    )(input)
  }

  fn parse_tuple_of_exprs(input: &str) -> AstParserResult {
    combinator::map(
      sequence::delimited(
        Self::ws_before(char('{')),
        Self::parse_comma_sep_exprs,
        Self::ws_before(char('}')),
      ),
      |elements| ErlAst::new_tuple(SourceLoc::None, elements),
    )(input)
  }

  fn parse_comma_sep_exprs(input: &str) -> nom::IResult<&str, Vec<Arc<ErlAst>>, ErlParserError> {
    multi::separated_list0(
      Self::ws_before(char(',')),
      // descend into precedence 11 instead of parse_expr, to ignore comma and semicolon
      Self::parse_expr_no_comma_no_semi,
    )(input)
  }

  fn parenthesized_expr(input: &str) -> AstParserResult {
    sequence::delimited(
      Self::ws_before(char('(')),
      Self::ws_before(Self::parse_expr),
      Self::ws_before(char(')')),
    )(input)
  }

  /// Priority 0: (Parenthesized expressions), numbers, variables, negation (unary ops)
  fn parse_prec_primary(input: &str) -> AstParserResult {
    Self::ws_before_mut(
      branch::alt((
        Self::parenthesized_expr,
        Self::parse_list_of_exprs,
        Self::parse_tuple_of_exprs,
        Self::parse_var,
        Self::parse_literal,
      ))
    )(input)
  }

  // TODO: Precedence 1: : (colon operator, for bit fields and module access?)
  // TODO: module:function notation and maybe tuple notation?
  /// Parse expr followed by a parentheses with 0 or more args, to become a function call
  fn parse_prec01(input: &str) -> AstParserResult {
    combinator::map(
      sequence::pair(
        Self::parse_prec_primary,
        combinator::opt(Self::parse_parenthesized_list_of_exprs),
      ),
      |(expr, args)| {
        match args {
          None => expr,
          Some(args_vec) => {
            let app = ErlApply {
              location: SourceLoc::None,
              expr,
              args: args_vec,
            };
            ErlAst::Apply(app).into()
          }
        }
      })(input)
  }

  // TODO: Precedence 2: # (record access operator)
  fn parse_prec02(input: &str) -> AstParserResult {
    Self::parse_prec01(input)
  }

  /// Precedence 3: Unary + - bnot not
  fn parse_prec03(input: &str) -> AstParserResult {
    combinator::map(
      sequence::pair(
        Self::ws_before_mut(branch::alt((
          Self::unop_negative, Self::unop_positive,
          Self::unop_bnot, Self::unop_not,
        ))),
        Self::ws_before(Self::parse_prec02),
      ),
      |(unop, expr)| ErlUnaryOperatorExpr::new_ast(SourceLoc::None, unop, expr),
    )(input)
        .or_else(|_err| Self::parse_prec02(input))
  }

  /// Precedence 4: / * div rem band and, left associative
  fn parse_prec04(input: &str) -> AstParserResult {
    combinator::map(
      // Higher precedence expr, followed by 0 or more operators and higher prec exprs
      sequence::pair(
        Self::parse_prec03,
        multi::many0(
          sequence::pair(
            Self::ws_before_mut(branch::alt((
              Self::binop_floatdiv, Self::binop_multiply,
              Self::binop_intdiv, Self::binop_rem,
              Self::binop_band, Self::binop_and,
            ))),
            Self::ws_before(Self::parse_prec03),
          )
        ),
      ),
      |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }

  /// Precedence 5: + - bor bxor bsl bsr or xor, left associative
  fn parse_prec05(input: &str) -> AstParserResult {
    combinator::map(
      // Higher precedence expr, followed by 0 or more operators and higher prec exprs
      sequence::tuple((
        Self::parse_prec04,
        multi::many0(
          sequence::pair(
            Self::ws_before_mut(branch::alt((
              Self::binop_add, Self::binop_subtract, Self::binop_bor,
              Self::binop_bxor, Self::binop_bsl, Self::binop_bsr,
              Self::binop_or, Self::binop_xor,
            ))),
            Self::ws_before(Self::parse_prec04),
          )
        )
      )),
      |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }

  /// Precedence 6: ++ --, right associative
  fn parse_prec06(input: &str) -> AstParserResult {
    combinator::map(
      // Higher precedence expr, followed by 0 or more operators and higher prec exprs
      sequence::pair(
        Self::parse_prec05,
        multi::many0(
          sequence::pair(
            Self::ws_before_mut(branch::alt((
              Self::binop_list_append, Self::binop_list_subtract,
            ))),
            Self::ws_before(Self::parse_prec05),
          )
        ),
      ),
      |(left, tail)| ErlBinaryOperatorExpr::new_right_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }

  /// Precedence 7: == /= =< < >= > =:= =/=
  fn parse_prec07(input: &str) -> AstParserResult {
    combinator::map(
      // Higher precedence expr, followed by 0 or more operators and higher prec exprs
      sequence::pair(
        Self::parse_prec06,
        multi::many0(
          sequence::pair(
            Self::ws_before_mut(branch::alt((
              Self::binop_equals, Self::binop_not_equals,
              Self::binop_less_eq, Self::binop_less,
              Self::binop_greater_eq, Self::binop_greater,
              Self::binop_hard_equals, Self::binop_hard_not_equals,
            ))),
            Self::ws_before(Self::parse_prec06),
          )
        ),
      ),
      |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }

  /// Precedence 8: andalso
  fn parse_prec08(input: &str) -> AstParserResult {
    combinator::map(
      // Higher precedence expr, followed by 0 or more ANDALSO operators and higher prec exprs
      sequence::pair(
        Self::parse_prec07,
        multi::many0(
          sequence::pair(
            Self::ws_before_mut(Self::binop_andalso),
            Self::ws_before(Self::parse_prec07),
          )
        ),
      ),
      |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }

  /// Precedence 9: orelse
  fn parse_prec09(input: &str) -> AstParserResult {
    combinator::map(
      // Higher precedence expr, followed by 0 or more ORELSE operators and higher prec exprs
      sequence::pair(
        Self::parse_prec08,
        multi::many0(
          sequence::pair(
            Self::ws_before_mut(Self::binop_orelse),
            Self::ws_before(Self::parse_prec08),
          )
        ),
      ),
      |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }

  /// Precedence 10: assignment/match = operator, and send operator "!", right associative
  fn parse_prec10(input: &str) -> AstParserResult {
    combinator::map(
      // Higher precedence expr, followed by 0 or more binary operators and higher prec exprs
      sequence::pair(
        Self::parse_prec09,
        multi::many0(
          sequence::pair(
            Self::ws_before_mut(branch::alt((
              Self::binop_match, Self::binop_bang
            ))),
            Self::ws_before(Self::parse_prec09),
          )
        ),
      ),
      |(left, tail)| ErlBinaryOperatorExpr::new_right_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }

  /// Precedence 11: Catch operator, then continue to higher precedences
  /// This is also entry point to parse expression when you don't want to recognize comma and semicolon
  fn parse_prec11(input: &str) -> AstParserResult {
    // Try parse (catch Expr) otherwise try next precedence level
    combinator::map(
      sequence::pair(
        Self::ws_before_mut(Self::unop_catch),
        Self::ws_before(Self::parse_prec10),
      ),
      |(catch_op, expr)| ErlUnaryOperatorExpr::new_ast(SourceLoc::None, catch_op, expr),
    )(input)
        .or_else(|_err| Self::ws_before(Self::parse_prec10)(input))
  }

  /// Public entry point to parse expression that cannot include comma or semicolon
  #[inline]
  pub fn parse_expr_no_comma_no_semi(input: &str) -> AstParserResult {
    Self::parse_prec11(input)
    // println!("parse_expr_no_comma_no_semi: {}", input);
    // match Self::parse_prec11(input) {
    //   Ok((tail, result)) => {
    //     println!("OK r={}", result.deref());
    //     Ok((tail, result))
    //   }
    //   Err(e) => Err(e)
    // }
  }

  /// Parse an expression.
  /// Lowest precedence 13, where we handle comma and semicolon as binary ops.
  /// Note that semicolon is not valid for regular code only allowed in guards.
  pub fn parse_expr(input: &str) -> AstParserResult {
    combinator::map(
      // Higher precedence expr, followed by 0 or more binary operators and higher prec exprs
      sequence::pair(
        Self::ws_before(Self::parse_prec11),
        multi::many0(
          sequence::pair(
            Self::ws_before_mut(branch::alt((
              Self::binop_semicolon, Self::binop_comma
            ))),
            Self::ws_before(Self::parse_prec11),
          )
        ),
      ),
      |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
    )(input)
  }
}