//! Use nom parser to parse a generic module attribute from a wall of text.
use nom::{combinator, sequence, branch, multi, combinator::{cut}, error::{context},
          character::complete::{char}, bytes, bytes::complete::{tag}};
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::nom_parse::{AstParserResult, ErlParser, ErlParserError, ParserResult};
use crate::erlang::syntax_tree::nom_parse::parse_atom::AtomParser;
use crate::erlang::syntax_tree::nom_parse::parse_type::ErlTypeParser;
use crate::mfarity::MFArity;
use crate::source_loc::SourceLoc;

/// Holds code for parsing `-attr ... .` for any kind of module attributes
pub struct ErlAttrParser {}

impl ErlAttrParser {
  // /// Matches ". <endline>" terminator for module attributes
  // pub fn attr_terminator(input: &str) -> VoidParserResult {
  //   combinator::map(
  //     ErlParser::ws_before(char('.')),
  //     |_| (),
  //   )(input)
  // }

  // fn parenthesized_attr_terminator(input: &str) -> VoidParserResult {
  //   sequence::preceded(
  //     ErlParser::ws_before(char(')')),
  //     Self::attr_terminator,
  //   )(input)
  // }

  // /// Consume attribute without parentheses, till `".\n"`
  // fn naked_attr(input: &str) -> nom::IResult<&str, &str, ErlParserError> {
  //   combinator::recognize(
  //     sequence::tuple((
  //       ErlParser::ws_before(char('-')),
  //       ErlParser::ws_before(ErlParser::parse_ident),
  //       multi::many_till(anychar, Self::attr_terminator)
  //     ))
  //   )(input)
  // }

  // /// Consume attribute with parentheses, from `"("` till `").\n"`
  // fn parenthesized_attr(input: &str) -> nom::IResult<&str, &str, ErlParserError> {
  //   combinator::recognize(
  //     sequence::tuple((
  //       ErlParser::ws_before(char('-')),
  //       ErlParser::ws_before(ErlParser::parse_ident),
  //       sequence::preceded(
  //         ErlParser::ws_before(char('(')),
  //         multi::many_till(anychar, Self::parenthesized_attr_terminator),
  //       ))))(input)
  // }

  /// Parses a generic `-TAG(TERM)."` attribute.
  /// Given a string, try and consume a generic attribute line starting with `-ident` and ending with
  /// a `"." NEWLINE`.
  pub fn parse_generic_attr(input: &str) -> AstParserResult {
    combinator::map(
      // Dash `-` and terminating `.` are matched outside by the caller.
      sequence::pair(
        ErlParser::ws_before(AtomParser::parse_atom),
        combinator::opt(
          sequence::delimited(
            ErlParser::ws_before(char('(')),
            ErlParser::parse_expr,
            ErlParser::ws_before(char(')')),
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
        ErlParser::ws_before(bytes::complete::tag("module")),
        sequence::delimited(
          ErlParser::ws_before(char('(')),
          ErlParser::ws_before(AtomParser::parse_atom),
          ErlParser::ws_before(char(')')),
        )),
      ErlAst::new_module_start_attr,
    )(input)
  }

  /// Parses a `fun/arity` atom with an integer.
  pub fn parse_funarity(input: &str) -> nom::IResult<&str, MFArity, ErlParserError> {
    combinator::map(
      sequence::tuple((
        AtomParser::parse_atom,
        char('/'),
        ErlParser::parse_int,
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
      ErlParser::ws_before(char('[')),
      multi::separated_list1(
        ErlParser::ws_before(char(',')),
        ErlParser::ws_before(Self::parse_funarity),
      ),
      ErlParser::ws_before(char(']')),
    )(input)
  }

  fn parse_export_mfa_list(input: &str) -> ParserResult<Vec<MFArity>> {
    sequence::delimited(
      ErlParser::ws_before(char('(')),
      ErlParser::ws_before(Self::parse_square_funarity_list1),
      ErlParser::ws_before(char(')')),
    )(input)
  }

  /// Parses an `-export([fn/arity, ...]).` attribute.
  /// Dash `-` and trailing `.` are matched outside by the caller.
  pub fn parse_export_attr(input: &str) -> AstParserResult {
    combinator::map(
      sequence::preceded(
        ErlParser::ws_before(bytes::complete::tag("export")),
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
        ErlParser::ws_before(bytes::complete::tag("export_type")),
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
        ErlParser::ws_before(bytes::complete::tag("import")),
        context("import attribute", cut(
          sequence::delimited(
            ErlParser::ws_before(char('(')),
            sequence::tuple((
              AtomParser::parse_atom,
              ErlParser::ws_before(char(',')),
              ErlParser::ws_before(Self::parse_square_funarity_list1),
            )),
            ErlParser::ws_before(char(')')),
          ),
        )),
      ),
      |(mod_name, _comma1, imports)| {
        ErlAst::new_import_attr(mod_name, imports)
      },
    )(input)
  }

  /// Parses a list of comma separated variables `(VAR1, VAR2, ...)`
  pub fn parse_parenthesized_list_of_vars(input: &str) -> nom::IResult<&str, Vec<String>, ErlParserError> {
    sequence::delimited(
      ErlParser::ws_before(char('(')),
      cut(
        multi::separated_list0(
          ErlParser::ws_before(char(',')),
          ErlTypeParser::parse_typevar_name,
        )
      ),
      ErlParser::ws_before(char(')')),
    )(input)
  }

  /// Parses a `-type IDENT(ARG, ...) :: TYPE.` attribute.
  /// Dash `-` and trailing `.` are matched outside by the caller.
  pub fn parse_type_attr(input: &str) -> AstParserResult {
    combinator::map(
      sequence::preceded(
        ErlParser::ws_before(tag("type")),
        sequence::tuple((
          ErlParser::ws_before(AtomParser::parse_atom),
          context("new type: type arguments", cut(
            Self::parse_parenthesized_list_of_vars)),
          ErlParser::ws_before(tag("::")),
          context("new type: type definition", cut(
            ErlTypeParser::parse_type)),
        )),
      ),
      |(type_name, type_args, _coloncolon, new_type)| {
        ErlAst::new_type_attr(type_name, type_args, new_type)
      },
    )(input)
  }

  /// Any module attribute goes here
  pub fn parse(input: &str) -> AstParserResult {
    sequence::terminated(
      sequence::preceded(
        ErlParser::ws_before(char('-')),
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
      ErlParser::ws_before(char('.')),
      // newline,
      // ),
    )(input)
  }
}
