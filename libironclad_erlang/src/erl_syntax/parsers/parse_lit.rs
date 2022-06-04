//! Parse literal values as the occur in source code

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::node_impl::ErlAstType::Lit;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::defs::{ParserInput, ParserResult};
use crate::erl_syntax::parsers::misc::{parse_float, parse_int};
use crate::erl_syntax::parsers::parse_atom::AtomParser;
use crate::erl_syntax::parsers::parse_str::StringParser;
use crate::erl_syntax::parsers::ErlParser;
use crate::literal::Literal;
use libironclad_error::source_loc::SourceLoc;
use nom::branch::alt;
use nom::combinator::map;

impl ErlParser {
  fn parse_string_to_ast(input: ParserInput) -> ParserResult<AstNode> {
    map(StringParser::parse_string, |s| {
      AstNodeImpl::construct_with_location(
        &SourceLoc::from_input(input),
        Lit { value: Literal::String(s).into() },
      )
    })(input)
  }

  fn parse_atom_to_ast(input: ParserInput) -> ParserResult<AstNode> {
    map(AtomParser::atom, |s| {
      AstNodeImpl::construct_with_location(
        &SourceLoc::from_input(input),
        Lit { value: Literal::Atom(s).into() },
      )
    })(input)
  }

  fn parse_float_to_ast(input: ParserInput) -> ParserResult<AstNode> {
    map(parse_float, |s| {
      let lit_node = Lit {
        value: Literal::Float(s.parse::<f64>().unwrap()).into(),
      };
      AstNodeImpl::construct_with_location(&SourceLoc::from_input(input), lit_node)
    })(input)
  }

  fn parse_int_to_ast(input: ParserInput) -> ParserResult<AstNode> {
    map(parse_int, |erl_int| {
      let lit_node = Lit {
        // TODO: Can parsed integer create a parse error?
        value: Literal::Integer(erl_int).into(),
      };
      AstNodeImpl::construct_with_location(&SourceLoc::from_input(input), lit_node)
    })(input)
  }

  /// Read a literal value from input string
  pub fn parse_literal(input: ParserInput) -> ParserResult<AstNode> {
    alt((
      Self::parse_float_to_ast,
      Self::parse_int_to_ast,
      Self::parse_atom_to_ast,
      Self::parse_string_to_ast,
    ))(input)
  }
}
