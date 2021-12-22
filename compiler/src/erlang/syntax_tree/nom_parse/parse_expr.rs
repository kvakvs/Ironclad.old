//! Parse expressions and guard expressions (with added ;, operators)

use std::sync::Arc;

use nom::{error, character, combinator, sequence, multi, bytes::complete::{tag}, branch};

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::node::erl_apply::ErlApply;
use crate::erlang::syntax_tree::node::erl_expression::{ErlBinaryOperatorExpr, ErlUnaryOperatorExpr};
use crate::erlang::syntax_tree::node::erl_var::ErlVar;
use crate::erlang::syntax_tree::nom_parse::{misc, parse_expr_op};
use crate::erlang::syntax_tree::nom_parse::misc::{parse_ident_capitalized};
use crate::erlang::syntax_tree::nom_parse::parse_lit::parse_literal;
use crate::source_loc::SourceLoc;

/// Parse a function call (application of args to a callable value)
fn parse_apply(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  // Application consists of a callable expression, "(", list of args, and ")"
  combinator::map(
    sequence::tuple((
      parse_expr,
      misc::ws(character::complete::char('(')),
      multi::separated_list0(misc::ws(tag(",")), parse_expr),
      misc::ws(character::complete::char(')')),
    )),
    |(expr, _, args, _)| {
      let application = ErlApply::new(SourceLoc::None, expr, args);
      ErlAst::Apply(application).into()
    },
  )(input)
}

fn parse_var(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  let (input, name) = parse_ident_capitalized(input)?;
  let ast = ErlAst::Var(ErlVar::new(SourceLoc::None, &name)).into();
  Ok((input, ast))
}

/// Parses a list of comma separated expressions in (parentheses)
pub fn parse_parenthesized_list(input: &str) -> nom::IResult<&str, Vec<Arc<ErlAst>>> {
  let (input, _) = misc::ws(character::complete::char('('))(input)?;
  combinator::map(
    sequence::pair(
      parse_comma_sep_exprs,
      misc::ws_before(character::complete::char(')')),
    ),
    |(elements, _)| elements,
  )(input)
}

fn parse_list(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  // TODO: list tail with |
  let (input, _) = misc::ws(character::complete::char('['))(input)?;
  combinator::map(
    sequence::pair(
      parse_comma_sep_exprs,
      misc::ws(character::complete::char(']')),
    ),
    // TODO: Unwrap comma tree into a vector of elements
    |(elements, _)| ErlAst::new_list(SourceLoc::None, elements),
  )(input)
}

fn parse_tuple(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  let (input, _) = misc::ws(character::complete::char('{'))(input)?;
  combinator::map(
    sequence::pair(
      parse_comma_sep_exprs,
      misc::ws(character::complete::char('}')),
    ),
    // TODO: Unwrap comma tree into a vector of elements
    |(elements, _)| ErlAst::new_tuple(SourceLoc::None, elements),
  )(input)
}

// fn parse_record(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {}

// fn parse_map(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {}

fn parse_comma_sep_exprs(input: &str) -> nom::IResult<&str, Vec<Arc<ErlAst>>> {
  multi::separated_list0(
    misc::ws(character::complete::char(',')),
    parse_expr)(input)
}

fn parenthesized_expr(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  let (input, _opening) = character::complete::char('(')(input)?;
  let (input, expr) = misc::ws(parse_expr)(input)?;
  let (input, _closing) = error::context(
    "closing paren",
    combinator::cut(
      misc::ws(character::complete::char(')'))),
  )(input)?;

  Ok((input, expr))
}

/// Priority 0: (Parenthesized expressions), numbers, variables, negation (unary ops)
fn primary(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  misc::ws_before_mut(
    branch::alt((
      parenthesized_expr, parse_list, parse_tuple,
      parse_var,
      parse_literal,
    ))
  )(input)
}

// TODO: Precedence 1: : (colon operator, for bit fields and module access?)
/// Parse expr followed by a parentheses with 0 or more args, to become a function call
// TODO: module:function notation and maybe tuple notation?
fn parse_prec01(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  combinator::map(
    sequence::pair(
      primary,
      combinator::opt(parse_parenthesized_list),
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
fn parse_prec02(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  parse_prec01(input)
}

/// Precedence 3: Unary + - bnot not
fn parse_prec03(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  combinator::map(
    sequence::pair(
      misc::ws_before_mut(branch::alt((
        parse_expr_op::unop_negative, parse_expr_op::unop_positive,
        parse_expr_op::unop_bnot, parse_expr_op::unop_not,
      ))),
      misc::ws_before(parse_prec02),
    ),
    |(unop, expr)| ErlUnaryOperatorExpr::new_ast(SourceLoc::None, unop, expr),
  )(input)
      .or_else(|_err| parse_prec02(input))
}

/// Precedence 4: / * div rem band and, left associative
fn parse_prec04(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  combinator::map(
    // Higher precedence expr, followed by 0 or more operators and higher prec exprs
    sequence::pair(
      parse_prec03,
      multi::many0(
        sequence::pair(
          misc::ws_before_mut(branch::alt((
            parse_expr_op::binop_floatdiv, parse_expr_op::binop_multiply,
            parse_expr_op::binop_intdiv, parse_expr_op::binop_rem,
            parse_expr_op::binop_band, parse_expr_op::binop_and,
          ))),
          misc::ws_before(parse_prec03),
        )
      ),
    ),
    |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
  )(input)
}

/// Precedence 5: + - bor bxor bsl bsr or xor, left associative
fn parse_prec05(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  combinator::map(
    // Higher precedence expr, followed by 0 or more operators and higher prec exprs
    sequence::tuple((
      parse_prec04,
      multi::many0(
        sequence::pair(
          misc::ws_before_mut(branch::alt((
            parse_expr_op::binop_add, parse_expr_op::binop_subtract, parse_expr_op::binop_bor,
            parse_expr_op::binop_bxor, parse_expr_op::binop_bsl, parse_expr_op::binop_bsr,
            parse_expr_op::binop_or, parse_expr_op::binop_xor,
          ))),
          misc::ws_before(parse_prec04),
        )
      )
    )),
    |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
  )(input)
}

/// Precedence 6: ++ --, right associative
fn parse_prec06(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  combinator::map(
    // Higher precedence expr, followed by 0 or more operators and higher prec exprs
    sequence::pair(
      parse_prec05,
      multi::many0(
        sequence::pair(
          misc::ws_before_mut(branch::alt((
            parse_expr_op::binop_list_append, parse_expr_op::binop_list_subtract,
          ))),
          misc::ws_before(parse_prec05),
        )
      ),
    ),
    |(left, tail)| ErlBinaryOperatorExpr::new_right_assoc(&SourceLoc::None, left, &tail),
  )(input)
}

/// Precedence 7: == /= =< < >= > =:= =/=
fn parse_prec07(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  combinator::map(
    // Higher precedence expr, followed by 0 or more operators and higher prec exprs
    sequence::pair(
      parse_prec06,
      multi::many0(
        sequence::pair(
          misc::ws_before_mut(branch::alt((
            parse_expr_op::binop_equals, parse_expr_op::binop_not_equals,
            parse_expr_op::binop_less_eq, parse_expr_op::binop_less,
            parse_expr_op::binop_greater_eq, parse_expr_op::binop_greater,
            parse_expr_op::binop_hard_equals, parse_expr_op::binop_hard_not_equals,
          ))),
          misc::ws_before(parse_prec06),
        )
      ),
    ),
    |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
  )(input)
}

/// Precedence 8: andalso
fn parse_prec08(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  combinator::map(
    // Higher precedence expr, followed by 0 or more ANDALSO operators and higher prec exprs
    sequence::pair(
      parse_prec07,
      multi::many0(
        sequence::pair(
          misc::ws_before_mut(parse_expr_op::binop_andalso),
          misc::ws_before(parse_prec07),
        )
      ),
    ),
    |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
  )(input)
}

/// Precedence 9: orelse
fn parse_prec09(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  combinator::map(
    // Higher precedence expr, followed by 0 or more ORELSE operators and higher prec exprs
    sequence::pair(
      parse_prec08,
      multi::many0(
        sequence::pair(
          misc::ws_before_mut(parse_expr_op::binop_orelse),
          misc::ws_before(parse_prec08),
        )
      ),
    ),
    |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
  )(input)
}

/// Precedence 10: assignment/match = operator, and send operator "!", right associative
fn parse_prec10(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  combinator::map(
    // Higher precedence expr, followed by 0 or more binary operators and higher prec exprs
    sequence::pair(
      parse_prec09,
      multi::many0(
        sequence::pair(
          misc::ws_before_mut(branch::alt((
            parse_expr_op::binop_match, parse_expr_op::binop_bang
          ))),
          misc::ws_before(parse_prec09),
        )
      ),
    ),
    |(left, tail)| ErlBinaryOperatorExpr::new_right_assoc(&SourceLoc::None, left, &tail),
  )(input)
}

/// Precedence 11: Catch operator, then continue to higher precedences
pub fn parse_prec11(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  // Try parse (catch Expr) otherwise try next precedence level
  combinator::map(
    sequence::pair(
      misc::ws_before_mut(parse_expr_op::unop_catch),
      misc::ws_before(parse_prec10),
    ),
    |(catch_op, expr)| ErlUnaryOperatorExpr::new_ast(SourceLoc::None, catch_op, expr),
  )(input)
      .or_else(|_err| misc::ws_before(parse_prec10)(input))
}

// /// Precedence 12: Lists, tuples, maps, etc
// pub fn parse_prec12(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
//   // Try parse (catch Expr) otherwise try next precedence level
//   misc::ws_before_mut(branch::alt((
//     parse_list, parse_tuple,
//   )))(input)
//       .or_else(|_err| misc::ws_before(parse_prec11)(input))
// }

/// Parse an expression.
/// Lowest precedence 13, where we handle comma and semicolon as binary ops.
/// Note that semicolon is not valid for regular code only allowed in guards.
#[inline]
pub fn parse_expr(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  combinator::map(
    // Higher precedence expr, followed by 0 or more binary operators and higher prec exprs
    sequence::pair(
      misc::ws_before(parse_prec11),
      multi::many0(
        sequence::pair(
          misc::ws_before_mut(branch::alt((
            parse_expr_op::binop_semicolon, parse_expr_op::binop_comma
          ))),
          misc::ws_before(parse_prec11),
        )
      ),
    ),
    |(left, tail)| ErlBinaryOperatorExpr::new_left_assoc(&SourceLoc::None, left, &tail),
  )(input)
}