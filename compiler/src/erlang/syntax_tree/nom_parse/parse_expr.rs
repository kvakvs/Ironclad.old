//! Parse expressions and guard expressions (with added ;, operators)

use std::sync::Arc;

use nom::{combinator, sequence, multi, branch,
          bytes::complete::{tag}};

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::erl_op::ErlBinaryOp;
use crate::erlang::syntax_tree::node::erl_expression::ErlBinaryOperatorExpr;
use crate::erlang::syntax_tree::nom_parse::misc;
use crate::erlang::syntax_tree::nom_parse::parse_expr_op::{parse_binop_expr, parse_unop_expr};

/// Parse a function call (application of args to a callable value)
fn parse_apply(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {}

fn parse_lambda(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {}

fn parse_var(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {}

fn parse_literal(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {}

fn parse_list(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {}

fn parse_tuple(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {}

fn parse_record(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {}

fn parse_map(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {}

pub fn parse_expr(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  branch::alt((
    parse_unop_expr,
    parse_binop_expr,
    parse_apply,
    parse_lambda,
    parse_var,
    parse_literal,
    parse_list,
    parse_map,
    parse_tuple,
    parse_record,
  ))(input)
}

/// Same as `parse_expr` but includes also comma and semicolon operators.
pub fn parse_guard_expr(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {}
