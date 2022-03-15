//! Parses `case of` and clause branches
use nom::{bytes, combinator, sequence};

use crate::erlang::syntax_tree::node::erl_case_clause::ErlCaseClause;
use crate::erlang::syntax_tree::nom_parse::{ErlParser, ErlParserError};

impl ErlParser {
  /// Parses a `Pattern when Condition -> ...` branch of a `case` or a `try of`
  pub fn parse_case_clause(input: &str) -> nom::IResult<&str, ErlCaseClause, ErlParserError> {
    combinator::map(
      sequence::tuple((
        Self::parse_matchexpr,
        combinator::opt(
          sequence::preceded(
            Self::ws_before(bytes::complete::tag("when")),
            Self::parse_expr,
          ),
        ),
        // The body after ->
        sequence::preceded(
          Self::ws_before(bytes::complete::tag("->")),
          Self::parse_expr,
        )
      )),
      |(pattern, maybe_when, body)| {
        ErlCaseClause::new(pattern, maybe_when, body)
      },
    )(input)
  }
}
