//! Parse literal values as the occur in source code

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::node_impl::AstNodeType::Lit;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::defs::ParserInput;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{parse_float, parse_int};
use crate::erl_syntax::parsers::parse_strings::atom_literal::parse_atom;
use crate::erl_syntax::parsers::parse_strings::str_literal::parse_doublequot_string;
use crate::literal::Literal;
use nom::branch::alt;
use nom::combinator::map;

fn parse_string_to_ast(input: ParserInput) -> ParserResult<AstNode> {
  map(parse_doublequot_string, |s| {
    AstNodeImpl::construct_with_location(input.loc(), Lit { value: Literal::String(s).into() })
  })(input.clone())
}

fn parse_atom_to_ast(input: ParserInput) -> ParserResult<AstNode> {
  map(parse_atom, |s| {
    AstNodeImpl::construct_with_location(input.loc(), Lit { value: Literal::Atom(s).into() })
  })(input.clone())
}

fn parse_float_to_ast(input: ParserInput) -> ParserResult<AstNode> {
  map(parse_float, |s| {
    let lit_node = Lit {
      value: Literal::Float(s.parse::<f64>().unwrap()).into(),
    };
    AstNodeImpl::construct_with_location(input.loc(), lit_node)
  })(input.clone())
}

fn parse_int_to_ast(input: ParserInput) -> ParserResult<AstNode> {
  map(parse_int, |erl_int| {
    let lit_node = Lit {
      // TODO: Can parsed integer create a parse error?
      value: Literal::Integer(erl_int).into(),
    };
    AstNodeImpl::construct_with_location(input.loc(), lit_node)
  })(input.clone())
}

/// Read a literal value from input string
pub(crate) fn parse_erl_literal(input: ParserInput) -> ParserResult<AstNode> {
  alt((parse_float_to_ast, parse_int_to_ast, parse_atom_to_ast, parse_string_to_ast))(input)
}
