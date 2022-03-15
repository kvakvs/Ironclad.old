//! Parses `case of` and clause branches
use nom::{bytes, bytes::complete::{tag}, character::complete::{char}, error::{context},
          combinator, combinator::{cut}, sequence, multi};
use crate::erlang::syntax_tree::erl_ast::ErlAst;

use crate::erlang::syntax_tree::node::erl_case_clause::ErlCaseClause;
use crate::erlang::syntax_tree::nom_parse::{AstParserResult, ErlParser, ErlParserError};
use crate::source_loc::SourceLoc;

impl ErlParser {
  /// Parses a `MATCH_EXPR when GUARD_EXPR -> EXPR` branch of a `case` or a `try of`
  pub fn parse_case_clause(input: &str) -> nom::IResult<&str, ErlCaseClause, ErlParserError> {
    combinator::map(
      sequence::tuple((
        Self::parse_matchexpr,
        combinator::opt(
          sequence::preceded(
            Self::ws_before(bytes::complete::tag("when")),
            context("case clause guard expression", cut(Self::parse_expr)),
          ),
        ),
        // The body after ->
        sequence::preceded(
          Self::ws_before(bytes::complete::tag("->")),
          context("case clause body", cut(Self::parse_expr)),
        )
      )),
      |(pattern, maybe_when, body)| {
        ErlCaseClause::new(pattern, maybe_when, body)
      },
    )(input)
  }

  /// Parses `case EXPR of MATCH -> EXPR; ... end`
  pub fn parse_case_statement(input: &str) -> AstParserResult {
    combinator::map(
      sequence::tuple((
        sequence::delimited(
          Self::ws_before(tag("case")),
          context("case expression", cut(
            ErlParser::parse_expr)),
          Self::ws_before(tag("of")),
        ),
        multi::separated_list1(
          Self::ws_before(char(';')),
          context("case statement clause", cut(Self::parse_case_clause)),
        ),
        Self::ws_before(tag("end")),
      )),
      |(expr, clauses, _end0)| {
        ErlAst::new_case_statement(SourceLoc::None, expr, clauses)
      },
    )(input)
  }
}
