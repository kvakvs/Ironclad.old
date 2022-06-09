//! Parsing macro invocations

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::defs::{ParserInput, ParserResult};
use crate::erl_syntax::parsers::misc::ws_before;
use crate::erl_syntax::preprocessor::parsers::preprocessor::macro_ident;
use nom::character::complete::char;
use nom::combinator::map;
use nom::sequence::preceded;

/// Parse a `? <IDENT>` for a macro without arguments
fn macro_invocation_name(input: ParserInput) -> ParserResult<String> {
  preceded(ws_before(char('?')), macro_ident)(input)
}

/// Parse a macro invocation
pub fn parse_macro_invocation(input: ParserInput) -> ParserResult<AstNode> {
  map(macro_invocation_name, |n| AstNodeImpl::new_lit_atom(input.loc(), &n))(input.clone())
}
