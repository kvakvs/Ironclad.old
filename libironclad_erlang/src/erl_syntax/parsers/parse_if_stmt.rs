//! Parse code for `if COND -> EXPR; ... end`

use crate::erl_syntax::erl_ast::ErlAst;
use crate::erl_syntax::node::erl_if_clause::ErlIfClause;
use crate::erl_syntax::parsers::misc::{semicolon, ws_before};
use crate::erl_syntax::parsers::{AstParserResult, ErlParser, ErlParserError};
use libironclad_error::source_loc::SourceLoc;
use nom::combinator::map;
use nom::multi::separated_list1;
use nom::sequence::{pair, preceded, terminated};
use nom::{bytes, bytes::complete::tag, combinator::cut, error::context};

impl ErlParser {
  /// Parses `if COND -> EXPR; ... end`
  pub fn parse_if_statement(input: &str) -> AstParserResult {
    let (input, _) = ws_before(tag("if"))(input)?;

    context(
      "if block",
      cut(map(
        terminated(
          separated_list1(semicolon, context("if block clause", cut(Self::parse_if_clause))),
          ws_before(tag("end")),
        ),
        |clauses| ErlAst::new_if_statement(SourceLoc::None, clauses),
      )),
    )(input)
  }

  /// Parses a `Condition -> ...` branch of `if COND -> EXPR; ... end` statement
  pub fn parse_if_clause(input: &str) -> nom::IResult<&str, ErlIfClause, ErlParserError> {
    map(
      pair(
        Self::parse_expr,
        // The body after ->
        preceded(ws_before(bytes::complete::tag("->")), Self::parse_expr),
      ),
      |(cond, body)| ErlIfClause::new(cond, body),
    )(input)
  }
}
