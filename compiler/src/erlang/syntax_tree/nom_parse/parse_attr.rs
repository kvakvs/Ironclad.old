//! Use nom parser to parse a generic module attribute from a wall of text.
use std::sync::Arc;
use nom::{combinator, sequence, branch, multi,
          character::complete::{char, anychar},
          bytes};
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::nom_parse::{ErlParser, ErlParserError};
use crate::erlang::syntax_tree::nom_parse::parse_atom::AtomParser;
use crate::mfarity::MFArity;
use crate::source_loc::SourceLoc;

impl ErlParser {
  /// Matches ". <endline>" terminator for module attributes
  pub fn attr_terminator(input: &str) -> nom::IResult<&str, (), ErlParserError> {
    combinator::map(
      Self::ws(char('.')),
      |_| (),
    )(input)
  }

  fn parenthesized_attr_terminator(input: &str) -> nom::IResult<&str, (), ErlParserError> {
    sequence::preceded(
      Self::ws_before(char(')')),
      Self::attr_terminator,
    )(input)
  }

  /// Consume attribute without parentheses, till `".\n"`
  fn naked_attr(input: &str) -> nom::IResult<&str, &str, ErlParserError> {
    combinator::recognize(
      sequence::tuple((
        Self::ws_before(char('-')),
        Self::ws_before(Self::parse_ident),
        multi::many_till(anychar, Self::attr_terminator)
      ))
    )(input)
  }

  /// Consume attribute with parentheses, from `"("` till `").\n"`
  fn parenthesized_attr(input: &str) -> nom::IResult<&str, &str, ErlParserError> {
    combinator::recognize(
      sequence::tuple((
        Self::ws_before(char('-')),
        Self::ws_before(Self::parse_ident),
        Self::ws_before(char('(')),
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
          Self::attr_terminator)
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
        // TODO: Check whether attrs can contain %comments, and then simplify with just whitespace checks
        Self::ws_before(char('-')),
        Self::ws_before(bytes::complete::tag("module")),
        sequence::delimited(
          Self::ws_before(char('(')),
          Self::ws_before(AtomParser::parse_atom),
          Self::ws_before(char(')')),
        ),
        Self::attr_terminator,
      )),
      |(_dash, _, name, _terminator)| ErlAst::new_module_start_attr(name),
    )(input)
  }

  /// Parses a `fun/arity` atom with an integer.
  pub fn parse_funarity(input: &str) -> nom::IResult<&str, MFArity, ErlParserError> {
    combinator::map(
      sequence::tuple((
        AtomParser::parse_atom,
        char('/'),
        Self::parse_int,
      )),
      |(name, _slash, arity_s)| {
        let arity = arity_s.parse::<usize>().unwrap_or(0);
        MFArity::new_local_from_string(name, arity)
      },
    )(input)
  }

  /// Parse a `fun/arity, ...` comma-separated list, at least 1 element long
  fn parse_square_funarity_list1(input: &str) -> nom::IResult<&str, Vec<MFArity>, ErlParserError> {
    sequence::delimited(
      Self::ws_before(char('[')),
      multi::separated_list1(
        Self::ws_before(char(',')),
        Self::ws_before(Self::parse_funarity),
      ),
      Self::ws_before(char(']')),
    )(input)
  }

  /// Parses an `-export([fn/arity, ...]).` attribute
  pub fn parse_export_attr(input: &str) -> nom::IResult<&str, Arc<ErlAst>, ErlParserError> {
    combinator::map(
      sequence::tuple((
        // TODO: Check whether attrs can contain %comments, and then simplify with just whitespace checks
        Self::ws_before(char('-')),
        Self::ws_before(bytes::complete::tag("export")),
        sequence::delimited(
          Self::ws_before(char('(')),
          Self::ws_before(Self::parse_square_funarity_list1),
          Self::ws_before(char(')')),
        ),
        Self::attr_terminator,
      )),
      |(_dash, _export, exports, _term)| {
        ErlAst::new_export_attr(exports)
      },
    )(input)
  }

  /// Parses an `-import([fn/arity, ...]).` attribute
  pub fn parse_import_attr(input: &str) -> nom::IResult<&str, Arc<ErlAst>, ErlParserError> {
    combinator::map(
      sequence::tuple((
        // TODO: Check whether attrs can contain %comments, and then simplify with just whitespace checks
        Self::ws_before(char('-')),
        Self::ws_before(bytes::complete::tag("import")),
        sequence::delimited(
          Self::ws_before(char('(')),
          sequence::tuple((
            AtomParser::parse_atom,
            Self::ws_before(char(',')),
            Self::ws_before(Self::parse_square_funarity_list1),
          )),
          Self::ws_before(char(')')),
        ),
        Self::attr_terminator,
      )),
      |(_dash, _import, (mod_name, _comma1, imports), _term)| {
        ErlAst::new_import_attr(mod_name, imports)
      },
    )(input)
  }
}