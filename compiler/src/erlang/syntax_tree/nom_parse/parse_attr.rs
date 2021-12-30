//! Use nom parser to parse a generic module attribute from a wall of text.
use std::sync::Arc;
use nom::{combinator, sequence, branch, multi, character::complete::{anychar}, bytes::complete::{tag}, character};
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::nom_parse::{ErlParser, ErlParserError};
use crate::erlang::syntax_tree::nom_parse::parse_atom::AtomParser;
use crate::source_loc::SourceLoc;

impl ErlParser {
  /// Matches ". <endline>" terminator for module attributes
  pub fn attr_terminator(input: &str) -> nom::IResult<&str, &str, ErlParserError> {
    combinator::recognize(
      sequence::pair(
        Self::ws_before(character::complete::char('.')),
        Self::newline,
      )
    )(input)
  }

  fn parenthesized_attr_terminator(input: &str) -> nom::IResult<&str, &str, ErlParserError> {
    combinator::recognize(
      sequence::tuple((
        Self::ws_before(character::complete::char(')')),
        Self::ws_before(character::complete::char('.')),
        Self::newline
      ))
    )(input)
  }

  /// Consume attribute without parentheses, till `".\n"`
  fn naked_attr(input: &str) -> nom::IResult<&str, &str, ErlParserError> {
    combinator::recognize(
      sequence::tuple((
        Self::ws_before(character::complete::char('-')),
        Self::ws_before(Self::parse_ident),
        multi::many_till(anychar, Self::attr_terminator)
      ))
    )(input)
  }

  /// Consume attribute with parentheses, from `"("` till `").\n"`
  fn parenthesized_attr(input: &str) -> nom::IResult<&str, &str, ErlParserError> {
    combinator::recognize(
      sequence::tuple((
        Self::ws_before(character::complete::char('-')),
        Self::ws_before(Self::parse_ident),
        Self::ws_before(character::complete::char('(')),
        multi::many_till(anychar, Self::parenthesized_attr_terminator)
      ))
    )(input)
  }

  /// Parses a generic `- "something" ... ".\n"` attribute, consuming everything as a string
  /// Given a string, try and consume a generic attribute line starting with `-ident` and ending with
  /// a `"." NEWLINE`
  pub fn parse_generic_attr(input: &str) -> nom::IResult<&str, Arc<ErlAst>, ErlParserError> {
    combinator::map(
      combinator::recognize(
        sequence::terminated(
          branch::alt((Self::naked_attr, Self::parenthesized_attr)),
          Self::newline)
      ),
      |attr| {
        let ast_node = ErlAst::UnparsedAttr {
          location: SourceLoc::None,
          text: attr.to_string(),
        };
        ast_node.into()
      })(input)
  }

  /// Parses a `-module(atom).` attribute
  pub fn parse_module_attr(input: &str) -> nom::IResult<&str, Arc<ErlAst>, ErlParserError> {
    combinator::map(
      sequence::tuple((
        Self::ws_before(character::complete::char('-')),
        Self::ws_before(tag("module")),
        Self::ws_before(character::complete::char('(')),
        Self::ws_before(AtomParser::parse_atom),
        Self::ws_before(character::complete::char(')')),
        Self::attr_terminator,
      )),
      |(_, _, _, name, _, _)| ErlAst::ModuleStartAttr {
        location: SourceLoc::None,
        name: name.to_string(),
      }.into(),
    )(input)
  }
}