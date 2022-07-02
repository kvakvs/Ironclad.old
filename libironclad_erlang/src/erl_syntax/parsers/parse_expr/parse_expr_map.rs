//! Parse maps in expressions

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_map::MapBuilderMember;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{
  tok_assign, tok_comma, tok_curly_close, tok_curly_open, tok_hash, tok_right_darr,
};
use crate::erl_syntax::parsers::parse_expr;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::source_loc::SourceLoc;
use nom::branch::alt;
use nom::combinator::map;
use nom::multi::separated_list0;
use nom::sequence::{delimited, pair, separated_pair};

/// Parse assignment in a map builder `keyExpr "=>" valueExpr`
fn map_builder_assign(input: ParserInput) -> ParserResult<MapBuilderMember> {
  map(
    separated_pair(parse_expr::parse_constant_expr, tok_right_darr, parse_expr::parse_expr),
    |(key, expr)| MapBuilderMember::new_assign(key, expr),
  )(input)
}

/// Parse match in a map builder `keyExpr ":=" matchExpr`
fn map_builder_match(input: ParserInput) -> ParserResult<MapBuilderMember> {
  map(
    separated_pair(parse_expr::parse_constant_expr, tok_assign, parse_expr::parse_expr),
    |(key, expr)| MapBuilderMember::new_match(key, expr),
  )(input)
}

/// Parse a map builder expression, which uses `=>` to assign the values.
/// Contrary to a map matcher, which would use `:=`.
pub fn parse_map_builder(input: ParserInput) -> ParserResult<AstNode> {
  map(
    delimited(
      pair(tok_hash, tok_curly_open),
      separated_list0(tok_comma, alt((map_builder_assign, map_builder_match))),
      tok_curly_close,
    ),
    |members| AstNodeImpl::new_map_builder(SourceLoc::new(&input), members),
  )(input.clone())
}
