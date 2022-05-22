//! Parses `case of` and clause branches
use nom::{bytes, bytes::complete::{tag}, character::complete::{char}, error::{context},
          combinator, combinator::{cut}, sequence, multi};
use libironclad_error::source_loc::SourceLoc;
use crate::syntax_tree::erl_ast::ErlAst;
use crate::syntax_tree::node::erl_case_clause::ErlCaseClause;
use crate::syntax_tree::nom_parse::{AstParserResult, ErlParser, ErlParserError};
use crate::syntax_tree::nom_parse::misc::MiscParser;

impl ErlParser {
  /// Parses a `MATCH_EXPR when GUARD_EXPR -> EXPR` branch of a `case` or a `try of`
  pub fn parse_case_clause(input: &str) -> nom::IResult<&str, ErlCaseClause, ErlParserError> {
    combinator::map(
      sequence::tuple((
        Self::parse_matchexpr,
        combinator::opt(
          sequence::preceded(
            MiscParser::ws_before(bytes::complete::tag("when")),
            context("case clause guard expression", cut(Self::parse_expr)),
          ),
        ),
        // The body after ->
        sequence::preceded(
          MiscParser::ws_before(bytes::complete::tag("->")),
          context("case clause body", cut(
            Self::parse_comma_sep_exprs1::<{ErlParser::EXPR_STYLE_FULL}>
          )),
        )
      )),
      |(pattern, maybe_when, body)| {
        let loc = SourceLoc::None;
        ErlCaseClause::new(pattern, maybe_when,
                           ErlAst::new_comma_expr(loc, body))
      },
    )(input)
  }

  /// Parses `case EXPR of MATCH -> EXPR; ... end`
  pub fn parse_case_statement(input: &str) -> AstParserResult {
    let (input, _) = MiscParser::ws_before(tag("case"))(input)?;

    context("case block", cut(
      combinator::map(
        sequence::tuple((
          sequence::terminated(
            ErlParser::parse_expr,
            MiscParser::ws_before(tag("of")),
          ),
          multi::separated_list1(
            MiscParser::ws_before(char(';')),
            context("case block clause", cut(
              Self::parse_case_clause
            )),
          ),
          MiscParser::ws_before(tag("end")),
        )),
        |(expr, clauses, _end0)| {
          ErlAst::new_case_statement(SourceLoc::None, expr, clauses)
        },
      )))(input)
  }
}
