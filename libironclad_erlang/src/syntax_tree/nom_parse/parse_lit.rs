//! Parse literal values as the occur in source code

use crate::literal::Literal;
use crate::syntax_tree::erl_ast::ErlAst;
use crate::syntax_tree::erl_ast::ErlAstType::Lit;
use crate::syntax_tree::nom_parse::misc::{parse_float, parse_int};
use crate::syntax_tree::nom_parse::parse_atom::AtomParser;
use crate::syntax_tree::nom_parse::parse_str::StringParser;
use crate::syntax_tree::nom_parse::{AstParserResult, ErlParser};
use ::function_name::named;
use libironclad_error::source_loc::SourceLoc;
use nom::branch::alt;
use nom::combinator::map;

impl ErlParser {
  #[named]
  fn parse_string_to_ast(input: &str) -> AstParserResult {
    map(StringParser::parse_string, |s| {
      ErlAst::construct_with_location(
        SourceLoc::unimplemented(file!(), function_name!()),
        Lit { value: Literal::String(s).into() },
      )
    })(input)
  }

  #[named]
  fn parse_atom_to_ast(input: &str) -> AstParserResult {
    map(AtomParser::parse_atom, |s| {
      ErlAst::construct_with_location(
        SourceLoc::unimplemented(file!(), function_name!()),
        Lit { value: Literal::Atom(s).into() },
      )
    })(input)
  }

  #[named]
  fn parse_float_to_ast(input: &str) -> AstParserResult {
    map(parse_float, |s| {
      let lit_node = Lit {
        value: Literal::Float(s.parse::<f64>().unwrap()).into(),
      };
      ErlAst::construct_with_location(SourceLoc::unimplemented(file!(), function_name!()), lit_node)
    })(input)
  }

  #[named]
  fn parse_int_to_ast(input: &str) -> AstParserResult {
    map(parse_int, |s| {
      let lit_node = Lit {
        value: Literal::Integer(s.parse::<isize>().unwrap()).into(),
      };
      ErlAst::construct_with_location(SourceLoc::unimplemented(file!(), function_name!()), lit_node)
    })(input)
  }

  /// Read a literal value from input string
  pub fn parse_literal(input: &str) -> AstParserResult {
    alt((
      Self::parse_float_to_ast,
      Self::parse_int_to_ast,
      Self::parse_atom_to_ast,
      Self::parse_string_to_ast,
    ))(input)
  }
}
