//! Parse code for `if COND -> EXPR; ... end`

use crate::syntax_tree::erl_ast::ErlAst;
use crate::syntax_tree::node::erl_if_clause::ErlIfClause;
use crate::syntax_tree::nom_parse::misc::ws_before;
use crate::syntax_tree::nom_parse::{AstParserResult, ErlParser, ErlParserError};
use libironclad_error::source_loc::SourceLoc;
use nom::combinator::map;
use nom::{
  bytes, bytes::complete::tag, character::complete::char, combinator::cut, error::context, multi,
  sequence,
};

impl ErlParser {
  /// Parses `if COND -> EXPR; ... end`
  pub fn parse_if_statement(input: &str) -> AstParserResult {
    let (input, _) = ws_before(tag("if"))(input)?;

    context(
      "if block",
      cut(map(
        sequence::terminated(
          multi::separated_list1(
            ws_before(char(';')),
            context("if block clause", cut(Self::parse_if_clause)),
          ),
          ws_before(tag("end")),
        ),
        |clauses| ErlAst::new_if_statement(SourceLoc::None, clauses),
      )),
    )(input)
  }

  /// Parses a `Condition -> ...` branch of `if COND -> EXPR; ... end` statement
  pub fn parse_if_clause(input: &str) -> nom::IResult<&str, ErlIfClause, ErlParserError> {
    map(
      sequence::pair(
        Self::parse_expr,
        // The body after ->
        sequence::preceded(ws_before(bytes::complete::tag("->")), Self::parse_expr),
      ),
      |(cond, body)| ErlIfClause::new(cond, body),
    )(input)
  }
}
