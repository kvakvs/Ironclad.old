//! Parse expressions and guard expressions (with added ;, operators)

use std::sync::Arc;

use nom::{combinator, sequence, multi, branch,
          bytes::complete::{tag}};

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::node::erl_apply::ErlApply;
use crate::erlang::syntax_tree::node::erl_var::ErlVar;
use crate::erlang::syntax_tree::nom_parse::misc;
use crate::erlang::syntax_tree::nom_parse::misc::parse_ident_capitalized;
use crate::erlang::syntax_tree::nom_parse::parse_expr_op::{parse_binop_expr, parse_unop_expr};
use crate::erlang::syntax_tree::nom_parse::parse_fn::parse_lambda;
use crate::erlang::syntax_tree::nom_parse::parse_lit::parse_literal;
use crate::source_loc::SourceLoc;

/// Parse a function call (application of args to a callable value)
fn parse_apply(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  // Application consists of a callable expression, "(", list of args, and ")"
  combinator::map(
    sequence::tuple((
      parse_expr,
      misc::ws(tag("(")),
      multi::separated_list0(misc::ws(tag(",")), parse_expr),
      misc::ws(tag(")")),
    )),
    |(expr, _, args, _)| {
      let application = ErlApply::new(SourceLoc::None, expr, args);
      ErlAst::Apply(application).into()
    },
  )(input)
}

fn parse_var(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  combinator::map(
    parse_ident_capitalized,
    |name| ErlAst::Var(ErlVar::new(SourceLoc::None, &name)).into(),
  )(input)
}

fn parse_list(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  // TODO: list tail with |
  combinator::map(
    sequence::tuple((
      misc::ws(tag("[")),
      parse_comma_sep_exprs,
      misc::ws(tag("]")),
    )),
    |(_, elements, _)| ErlAst::new_list(SourceLoc::None, elements),
  )(input)
}

fn parse_tuple(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  combinator::map(
    sequence::tuple((
      misc::ws(tag("{")),
      parse_comma_sep_exprs,
      misc::ws(tag("}")),
    )),
    |(_, elements, _)| ErlAst::new_tuple(SourceLoc::None, elements),
  )(input)
}

// fn parse_record(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {}

// fn parse_map(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {}

fn parse_comma_sep_exprs(input: &str) -> nom::IResult<&str, Vec<Arc<ErlAst>>> {
  multi::separated_list0(
    misc::ws(tag(",")),
    parse_expr)(input)
}

pub fn parse_expr_impl(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  branch::alt((
    parse_unop_expr,
    parse_binop_expr,
    parse_apply,
    parse_lambda,
    parse_var,
    parse_literal,
    parse_list,
    parse_tuple,
    // parse_map,
    //parse_record,
  ))(input)
}

pub fn parse_expr(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  // TODO: Check that it does not contain semicolon operator allowed in guards but not in other exprs
  parse_expr_impl(input)
}

/// Same as `parse_expr` but includes also comma and semicolon operators.
pub fn parse_guard_expr(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  parse_expr_impl(input)
}
