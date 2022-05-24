//! Parses `case of` and clause branches
use crate::syntax_tree::erl_ast::ErlAst;
use crate::syntax_tree::node::erl_case_clause::ErlCaseClause;
use crate::syntax_tree::nom_parse::misc::ws_before;
use crate::syntax_tree::nom_parse::{AstParserResult, ErlParser, ErlParserError};
use libironclad_error::source_loc::SourceLoc;
use nom::combinator::{map, opt};
use nom::{
  bytes, bytes::complete::tag, character::complete::char, combinator::cut, error::context, multi,
  sequence,
};

impl ErlParser {
  /// Parses a `MATCH_EXPR when GUARD_EXPR -> EXPR` branch of a `case` or a `try of`
  pub fn parse_case_clause(input: &str) -> nom::IResult<&str, ErlCaseClause, ErlParserError> {
    map(
      sequence::tuple((
        Self::parse_matchexpr,
        opt(sequence::preceded(
          ws_before(bytes::complete::tag("when")),
          context("case clause guard expression", cut(Self::parse_expr)),
        )),
        // The body after ->
        sequence::preceded(
          ws_before(bytes::complete::tag("->")),
          context(
            "case clause body",
            cut(Self::parse_comma_sep_exprs1::<{ ErlParser::EXPR_STYLE_FULL }>),
          ),
        ),
      )),
      |(pattern, maybe_when, body)| {
        let loc = SourceLoc::None;
        ErlCaseClause::new(pattern, maybe_when, ErlAst::new_comma_expr(loc, body))
      },
    )(input)
  }

  /// Parses `case EXPR of MATCH -> EXPR; ... end`
  pub fn parse_case_statement(input: &str) -> AstParserResult {
    let (input, _) = ws_before(tag("case"))(input)?;

    context(
      "case block",
      cut(map(
        sequence::tuple((
          sequence::terminated(ErlParser::parse_expr, ws_before(tag("of"))),
          multi::separated_list1(
            ws_before(char(';')),
            context("case block clause", cut(Self::parse_case_clause)),
          ),
          ws_before(tag("end")),
        )),
        |(expr, clauses, _end0)| ErlAst::new_case_statement(SourceLoc::None, expr, clauses),
      )),
    )(input)
  }
}
