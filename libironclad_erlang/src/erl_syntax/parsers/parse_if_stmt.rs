//! Parse code for `if COND -> EXPR; ... end`

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_if_clause::ErlIfClause;
use crate::erl_syntax::parsers::defs::ParserInput;
use crate::erl_syntax::parsers::defs::{ErlParserError, ParserResult};
use crate::erl_syntax::parsers::misc::{match_word, semicolon_tag, ws_before};
use crate::erl_syntax::parsers::ErlParser;
use nom::combinator::map;
use nom::multi::separated_list1;
use nom::sequence::{pair, preceded, terminated};
use nom::{bytes::complete::tag, combinator::cut, error::context};

impl ErlParser {
  /// Parses `if COND -> EXPR; ... end`
  pub fn parse_if_statement(input: ParserInput) -> ParserResult<AstNode> {
    preceded(
      match_word("if".into()),
      context(
        "if block",
        cut(map(
          terminated(
            separated_list1(semicolon_tag, context("if block clause", cut(Self::parse_if_clause))),
            ws_before(tag("end".into())),
          ),
          |clauses| AstNodeImpl::new_if_statement(input.loc(), clauses),
        )),
      ),
    )(input.clone())
  }

  /// Parses a `Condition -> ...` branch of `if COND -> EXPR; ... end` statement
  pub fn parse_if_clause(
    input: ParserInput,
  ) -> nom::IResult<ParserInput, ErlIfClause, ErlParserError> {
    map(
      pair(
        Self::parse_expr,
        // The body after ->
        preceded(ws_before(tag("->".into())), Self::parse_expr),
      ),
      |(cond, body)| ErlIfClause::new(cond, body),
    )(input)
  }
}
