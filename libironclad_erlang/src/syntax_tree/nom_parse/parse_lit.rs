//! Parse literal values as the occur in source code

use crate::literal::Literal;
use crate::syntax_tree::erl_ast::ErlAst;
use crate::syntax_tree::nom_parse::misc::{parse_float, parse_int};
use crate::syntax_tree::nom_parse::parse_atom::AtomParser;
use crate::syntax_tree::nom_parse::parse_str::StringParser;
use crate::syntax_tree::nom_parse::{AstParserResult, ErlParser};
use libironclad_error::source_loc::SourceLoc;
use nom::branch::alt;
use nom::combinator;

impl ErlParser {
  fn parse_string_to_ast(input: &str) -> AstParserResult {
    combinator::map(StringParser::parse_string, |s| {
      ErlAst::Lit {
        location: SourceLoc::None,
        value: Literal::String(s).into(),
      }
      .into()
    })(input)
  }

  fn parse_atom_to_ast(input: &str) -> AstParserResult {
    combinator::map(AtomParser::parse_atom, |s| {
      ErlAst::Lit {
        location: SourceLoc::None,
        value: Literal::Atom(s).into(),
      }
      .into()
    })(input)
  }

  fn parse_float_to_ast(input: &str) -> AstParserResult {
    combinator::map(parse_float, |s| {
      ErlAst::Lit {
        location: SourceLoc::None,
        value: Literal::Float(s.parse::<f64>().unwrap()).into(),
      }
      .into()
    })(input)
  }

  fn parse_int_to_ast(input: &str) -> AstParserResult {
    combinator::map(parse_int, |s| {
      ErlAst::Lit {
        location: SourceLoc::None,
        value: Literal::Integer(s.parse::<isize>().unwrap()).into(),
      }
      .into()
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
