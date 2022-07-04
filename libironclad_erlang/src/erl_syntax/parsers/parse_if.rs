//! Parse code for `if COND -> EXPR; ... end`

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_if_clause::ErlIfClause;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc_tok::*;
use crate::erl_syntax::parsers::parse_expr::{parse_expr, parse_guardexpr};
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::parsers::token_stream::token_type::TokenType;
use crate::source_loc::SourceLoc;
use nom::combinator::map;
use nom::multi::separated_list1;
use nom::sequence::{pair, preceded, separated_pair, terminated};
use nom::{combinator::cut, error::context};

/// Parses a `Condition -> ...` branch of `if COND -> EXPR; ... end` statement
fn parse_if_clause(input: ParserInput) -> ParserResult<ErlIfClause> {
  map(
    separated_pair(
      // The condition
      parse_guardexpr,
      // The arrow ->
      tok_right_arrow,
      // The body
      parse_expr,
    ),
    |(cond, body)| ErlIfClause::new(cond, body),
  )(input)
}

/// Parses `if COND -> EXPR; ... end`
pub(crate) fn parse_if_expression(input: ParserInput) -> ParserResult<AstNode> {
  preceded(
    tok_keyword_if,
    context(
      "if-end expression",
      cut(map(
        terminated(
          separated_list1(tok_semicolon, context("if block clause", parse_if_clause)),
          tok_keyword_end,
        ),
        |clauses| AstNodeImpl::new_if_statement(SourceLoc::new(&input), clauses),
      )),
    ),
  )(input.clone())
}
