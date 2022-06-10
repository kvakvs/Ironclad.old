//! Parses `case of` and clause branches
use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_case_clause::ErlCaseClause;
use crate::erl_syntax::parsers::defs::{ErlParserError, ParserInput, ParserResult};
use crate::erl_syntax::parsers::misc::{match_word, semicolon_tag, ws_before};
use crate::erl_syntax::parsers::parse_expr::{
  parse_comma_sep_exprs1, parse_expr, parse_matchexpr, ExprStyle,
};
use nom::combinator::{cut, map, opt};
use nom::multi::separated_list1;
use nom::sequence::{preceded, terminated, tuple};
use nom::{bytes::complete::tag, error::context};

/// Parses a `MATCH_EXPR when GUARD_EXPR -> EXPR` branch of a `case` or a `try of`
pub(crate) fn parse_case_clause(
  input: ParserInput,
) -> nom::IResult<ParserInput, ErlCaseClause, ErlParserError> {
  map(
    tuple((
      parse_matchexpr,
      opt(preceded(
        ws_before(tag("when".into())),
        context("case clause guard expression", cut(parse_expr)),
      )),
      // The body after ->
      preceded(
        ws_before(tag("->".into())),
        context("case clause body", cut(parse_comma_sep_exprs1::<{ ExprStyle::Full }>)),
      ),
    )),
    |(pattern, maybe_when, body)| {
      ErlCaseClause::new(pattern, maybe_when, AstNodeImpl::new_comma_expr(input.loc(), body))
    },
  )(input.clone())
}

/// Parses `case EXPR of MATCH -> EXPR; ... end`
pub(crate) fn parse_case_statement(input: ParserInput) -> ParserResult<AstNode> {
  // let (input, _) = ws_before(tag("case".into()))(input)?;

  preceded(
    match_word("case".into()),
    context(
      "case block",
      cut(map(
        tuple((
          terminated(parse_expr, ws_before(tag("of".into()))),
          separated_list1(semicolon_tag, context("case block clause", cut(parse_case_clause))),
          ws_before(tag("end".into())),
        )),
        |(expr, clauses, _end0)| AstNodeImpl::new_case_statement(input.loc(), expr, clauses),
      )),
    ),
  )(input.clone())
}
