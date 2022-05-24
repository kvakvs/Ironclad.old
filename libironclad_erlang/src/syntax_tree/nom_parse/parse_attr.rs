//! Use nom parser to parse a generic module attribute from a wall of text.
use crate::syntax_tree::erl_ast::ErlAst;
use crate::syntax_tree::nom_parse::misc::MiscParser;
use crate::syntax_tree::nom_parse::parse_atom::AtomParser;
use crate::syntax_tree::nom_parse::parse_type::ErlTypeParser;
use crate::syntax_tree::nom_parse::{AstParserResult, ErlParser, ErlParserError, ParserResult};
use libironclad_error::source_loc::SourceLoc;
use libironclad_util::mfarity::MFArity;
use nom::{
  branch, bytes, bytes::complete::tag, character::complete::char, combinator, combinator::cut, error::context, multi,
  sequence,
};

/// Holds code for parsing `-attr ... .` for any kind of module attributes
pub struct ErlAttrParser {}

impl ErlAttrParser {
  /// Parses a generic `-TAG(TERM)."` attribute.
  /// Given a string, try and consume a generic attribute line starting with `-ident` and ending with
  /// a `"." NEWLINE`.
  pub fn parse_generic_attr(input: &str) -> AstParserResult {
    combinator::map(
      // Dash `-` and terminating `.` are matched outside by the caller.
      sequence::pair(
        MiscParser::ws_before(AtomParser::parse_atom),
        combinator::opt(sequence::delimited(
          MiscParser::ws_before(char('(')),
          ErlParser::parse_expr,
          MiscParser::ws_before(char(')')),
        )),
      ),
      |(tag, term)| ErlAst::new_generic_attr(SourceLoc::None, tag, term),
    )(input)
  }

  /// Parses a `-module(atom).` attribute.
  /// Dash `-` and terminating `.` are matched outside by the caller.
  pub fn parse_module_attr(input: &str) -> AstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(tag("module")),
        sequence::delimited(
          MiscParser::ws_before(char('(')),
          MiscParser::ws_before(AtomParser::parse_atom),
          MiscParser::ws_before(char(')')),
        ),
      ),
      ErlAst::new_module_start_attr,
    )(input)
  }

  /// Parses a `fun/arity` atom with an integer.
  pub fn parse_funarity(input: &str) -> nom::IResult<&str, MFArity, ErlParserError> {
    combinator::map(
      sequence::tuple((AtomParser::parse_atom, char('/'), MiscParser::parse_int)),
      |(name, _slash, arity_s)| {
        let arity = arity_s.parse::<usize>().unwrap_or(0);
        MFArity::new_local_from_string(name, arity)
      },
    )(input)
  }

  /// Parse a `fun/arity, ...` comma-separated list, at least 1 element long
  fn parse_square_funarity_list1(input: &str) -> nom::IResult<&str, Vec<MFArity>, ErlParserError> {
    sequence::delimited(
      MiscParser::ws_before(char('[')),
      multi::separated_list1(MiscParser::ws_before(char(',')), MiscParser::ws_before(Self::parse_funarity)),
      MiscParser::ws_before(char(']')),
    )(input)
  }

  fn parse_export_mfa_list(input: &str) -> ParserResult<Vec<MFArity>> {
    sequence::delimited(
      MiscParser::ws_before(char('(')),
      MiscParser::ws_before(Self::parse_square_funarity_list1),
      MiscParser::ws_before(char(')')),
    )(input)
  }

  /// Parses an `-export([fn/arity, ...]).` attribute.
  /// Dash `-` and trailing `.` are matched outside by the caller.
  pub fn parse_export_attr(input: &str) -> AstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(bytes::complete::tag("export")),
        Self::parse_export_mfa_list,
      ),
      ErlAst::new_export_attr,
    )(input)
  }

  /// Parses an `-export_type([type/arity, ...]).` attribute.
  /// Dash `-` and trailing `.` are matched outside by the caller.
  pub fn parse_export_type_attr(input: &str) -> AstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(bytes::complete::tag("export_type")),
        Self::parse_export_mfa_list,
      ),
      ErlAst::new_export_type_attr,
    )(input)
  }

  /// Parses an `-import(module [fn/arity, ...]).` attribute.
  /// Dash `-` and trailing `.` are matched outside by the caller.
  pub fn parse_import_attr(input: &str) -> AstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(bytes::complete::tag("import")),
        context(
          "import attribute",
          cut(sequence::delimited(
            MiscParser::ws_before(char('(')),
            sequence::tuple((
              AtomParser::parse_atom,
              MiscParser::ws_before(char(',')),
              MiscParser::ws_before(Self::parse_square_funarity_list1),
            )),
            MiscParser::ws_before(char(')')),
          )),
        ),
      ),
      |(mod_name, _comma1, imports)| ErlAst::new_import_attr(mod_name, imports),
    )(input)
  }

  /// Parses a list of comma separated variables `(VAR1, VAR2, ...)`
  pub fn parse_parenthesized_list_of_vars(input: &str) -> nom::IResult<&str, Vec<String>, ErlParserError> {
    sequence::delimited(
      MiscParser::ws_before(char('(')),
      cut(multi::separated_list0(
        MiscParser::ws_before(char(',')),
        ErlTypeParser::parse_typevar_name,
      )),
      MiscParser::ws_before(char(')')),
    )(input)
  }

  /// Parses a `-type IDENT(ARG, ...) :: TYPE.` attribute.
  /// Dash `-` and trailing `.` are matched outside by the caller.
  pub fn parse_type_attr(input: &str) -> AstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(tag("type")),
        sequence::tuple((
          MiscParser::ws_before(AtomParser::parse_atom),
          context("new type: type arguments", cut(Self::parse_parenthesized_list_of_vars)),
          MiscParser::ws_before(tag("::")),
          context("new type: type definition", cut(ErlTypeParser::parse_type)),
        )),
      ),
      |(type_name, type_args, _coloncolon, new_type)| ErlAst::new_type_attr(type_name, type_args, new_type),
    )(input)
  }

  /// Any module attribute goes here
  pub fn parse(input: &str) -> AstParserResult {
    sequence::terminated(
      sequence::preceded(
        MiscParser::ws_before(char('-')),
        branch::alt((
          Self::parse_export_type_attr,
          Self::parse_export_attr,
          Self::parse_import_attr,
          Self::parse_type_attr,
          Self::parse_module_attr,
          ErlTypeParser::parse_fn_spec,
          // Generic parser will try consume any `-IDENT(EXPR).`
          Self::parse_generic_attr,
        )),
      ),
      // sequence::terminated(
      MiscParser::ws_before(char('.')),
      // newline,
      // ),
    )(input)
  }
}
