//! Parses `case of` and clause branches
use crate::erl_syntax::erl_ast::ErlAst;
use crate::erl_syntax::node::erl_case_clause::ErlCaseClause;
use crate::erl_syntax::parsers::misc::{semicolon, ws_before};
use crate::erl_syntax::parsers::{AstParserResult, ErlParser, ErlParserError};
use libironclad_error::source_loc::SourceLoc;
use nom::combinator::{cut, map, opt};
use nom::multi::separated_list1;
use nom::sequence::{preceded, terminated, tuple};
use nom::{bytes, bytes::complete::tag, error::context};

impl ErlParser {
  /// Parses a `MATCH_EXPR when GUARD_EXPR -> EXPR` branch of a `case` or a `try of`
  pub fn parse_case_clause(input: &str) -> nom::IResult<&str, ErlCaseClause, ErlParserError> {
    map(
      tuple((
        Self::parse_matchexpr,
        opt(preceded(
          ws_before(bytes::complete::tag("when")),
          context("case clause guard expression", cut(Self::parse_expr)),
        )),
        // The body after ->
        preceded(
          ws_before(bytes::complete::tag("->")),
          context(
            "case clause body",
            cut(Self::parse_comma_sep_exprs1::<{ ErlParser::EXPR_STYLE_FULL }>),
          ),
        ),
      )),
      |(pattern, maybe_when, body)| {
        ErlCaseClause::new(
          pattern,
          maybe_when,
          ErlAst::new_comma_expr(&SourceLoc::from_input(input), body),
        )
      },
    )(input)
  }

  /// Parses `case EXPR of MATCH -> EXPR; ... end`
  pub fn parse_case_statement(input: &str) -> AstParserResult {
    let (input, _) = ws_before(tag("case"))(input)?;

    context(
      "case block",
      cut(map(
        tuple((
          terminated(ErlParser::parse_expr, ws_before(tag("of"))),
          separated_list1(semicolon, context("case block clause", cut(Self::parse_case_clause))),
          ws_before(tag("end")),
        )),
        |(expr, clauses, _end0)| {
          ErlAst::new_case_statement(&SourceLoc::from_input(input), expr, clauses)
        },
      )),
    )(input)
  }
}
