//! Parse code for `if COND -> EXPR; ... end`

use nom::{bytes, bytes::complete::{tag}, character::complete::{char}, error::{context},
          combinator, combinator::{cut}, sequence, multi};
use libironclad_error::source_loc::SourceLoc;
use crate::syntax_tree::erl_ast::ErlAst;
use crate::syntax_tree::node::erl_if_clause::ErlIfClause;
use crate::syntax_tree::nom_parse::{AstParserResult, ErlParser, ErlParserError};
use crate::syntax_tree::nom_parse::misc::MiscParser;

impl ErlParser {
  /// Parses `if COND -> EXPR; ... end`
  pub fn parse_if_statement(input: &str) -> AstParserResult {
    let (input, _) = MiscParser::ws_before(tag("if"))(input)?;

    context("if block", cut(
      combinator::map(
        sequence::terminated(
          multi::separated_list1(
            MiscParser::ws_before(char(';')),
            context("if block clause", cut(Self::parse_if_clause)),
          ),
          MiscParser::ws_before(tag("end")),
        ),
        |clauses| ErlAst::new_if_statement(SourceLoc::None, clauses),
      )))(input)
  }

  /// Parses a `Condition -> ...` branch of `if COND -> EXPR; ... end` statement
  pub fn parse_if_clause(input: &str) -> nom::IResult<&str, ErlIfClause, ErlParserError> {
    combinator::map(
      sequence::pair(
        Self::parse_expr,

        // The body after ->
        sequence::preceded(
          MiscParser::ws_before(bytes::complete::tag("->")),
          Self::parse_expr,
        ),
      ),
      |(cond, body)| ErlIfClause::new(cond, body),
    )(input)
  }
}