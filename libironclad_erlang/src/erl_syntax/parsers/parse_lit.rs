//! Parse literal values as the occur in source code

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::node_impl::AstNodeType::Lit;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{tok_atom, tok_float, tok_integer, tok_string, ws_before};
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::literal::Literal;
use crate::source_loc::SourceLoc;
use crate::typing::erl_integer::ErlInteger;
use nom::branch::alt;
use nom::combinator::map;
use nom::error::context;

fn parse_string_to_ast(input: ParserInput) -> ParserResult<AstNode> {
  map(tok_string, |s| {
    AstNodeImpl::construct_with_location(
      SourceLoc::new(&input),
      Lit { value: Literal::String(s).into() },
    )
  })(input.clone())
}

fn parse_atom_to_ast(input: ParserInput) -> ParserResult<AstNode> {
  map(tok_atom, |s| {
    AstNodeImpl::construct_with_location(
      SourceLoc::new(&input),
      Lit { value: Literal::Atom(s).into() },
    )
  })(input.clone())
}

fn parse_float_to_ast(input: ParserInput) -> ParserResult<AstNode> {
  map(tok_float, |f: f64| {
    let lit_node = Lit { value: Literal::Float(f).into() };
    AstNodeImpl::construct_with_location(SourceLoc::new(&input), lit_node)
  })(input.clone())
}

fn parse_int_to_ast(input: ParserInput) -> ParserResult<AstNode> {
  map(tok_integer, |i: ErlInteger| {
    let lit_node = Lit {
      // TODO: Can parsed integer create a parse error?
      value: Literal::Integer(i).into(),
    };
    AstNodeImpl::construct_with_location(SourceLoc::new(&input), lit_node)
  })(input.clone())
}

/// Read a literal value from input string
pub(crate) fn parse_erl_literal(input: ParserInput) -> ParserResult<AstNode> {
  context(
    "literal",
    ws_before(alt((
      parse_float_to_ast,
      parse_int_to_ast,
      parse_atom_to_ast,
      parse_string_to_ast,
    ))),
  )(input)
}
