//! Parse code for `if COND -> EXPR; ... end`

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_if_clause::ErlIfClause;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{tok, tok_keyword_end, tok_keyword_if, tok_semicolon};
use crate::erl_syntax::parsers::parse_expr::parse_expr;
use crate::erl_syntax::parsers::parser_error::ErlParserError;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::source_loc::SourceLoc;
use nom::combinator::map;
use nom::multi::separated_list1;
use nom::sequence::{pair, preceded, terminated};
use nom::{combinator::cut, error::context};

/// Parses `if COND -> EXPR; ... end`
pub(crate) fn parse_if_statement(input: ParserInput) -> ParserResult<AstNode> {
  preceded(
    tok_keyword_if,
    context(
      "if block",
      cut(map(
        terminated(
          separated_list1(tok_semicolon, context("if block clause", cut(parse_if_clause))),
          tok_keyword_end,
        ),
        |clauses| AstNodeImpl::new_if_statement(SourceLoc::new(&input), clauses),
      )),
    ),
  )(input.clone())
}

/// Parses a `Condition -> ...` branch of `if COND -> EXPR; ... end` statement
pub(crate) fn parse_if_clause(
  input: ParserInput,
) -> nom::IResult<ParserInput, ErlIfClause, ErlParserError> {
  map(
    pair(
      parse_expr,
      // The body after ->
      preceded(tok(TokenType::RightArr), parse_expr),
    ),
    |(cond, body)| ErlIfClause::new(cond, body),
  )(input)
}
