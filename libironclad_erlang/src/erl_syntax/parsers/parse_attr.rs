//! Use nom parser to parse a generic module attribute from a wall of text.
use crate::erl_syntax::erl_ast::ErlAst;
use crate::erl_syntax::node::erl_record::RecordField;
use crate::erl_syntax::parsers::misc::{
  colon_colon, comma, curly_close, curly_open, equals_sign, match_dash_tag, par_close, par_open,
  parse_int, period_newline, print_input, square_close, square_open, ws_before,
};
use crate::erl_syntax::parsers::parse_atom::AtomParser;
use crate::erl_syntax::parsers::parse_type::ErlTypeParser;
use crate::erl_syntax::parsers::{AstParserResult, ErlParser, ErlParserError, ParserResult};
use libironclad_error::source_loc::SourceLoc;
use libironclad_util::mfarity::MFArity;
use nom::branch::alt;
use nom::combinator::{cut, map, opt};
use nom::multi::{separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, separated_pair, tuple};
use nom::{character::complete::char, error::context};
use std::sync::Arc;

/// Holds code for parsing `-attr ... .` for any kind of module attributes
pub struct ErlAttrParser {}

impl ErlAttrParser {
  /// Parse a `()` for a generic attribute `-<atom>().` and return empty `ErlAst`
  fn parse_parentheses_no_expr(input: &str) -> ParserResult<Option<Arc<ErlAst>>> {
    map(pair(par_open, par_close), |_| None)(input)
  }

  /// Parse a `( EXPR )` for a generic attribute `-<atom> ( EXPR ).`
  fn parse_generic_attr_expr(input: &str) -> ParserResult<Option<Arc<ErlAst>>> {
    map(
      delimited(
        par_open,
        context("an expression inside a custom -<name>() attribute", cut(ErlParser::parse_expr)),
        par_close,
      ),
      Option::Some,
    )(input)
  }

  /// Parses a generic `-TAG(TERM)."` attribute.
  /// Given a string, try and consume a generic attribute line starting with `-ident` and ending with
  /// a `"." NEWLINE`.
  pub fn parse_generic_attr(input: &str) -> AstParserResult {
    map(
      delimited(
        ws_before(char('-')),
        pair(
          AtomParser::atom,
          // Expr in parentheses
          alt((Self::parse_generic_attr_expr, Self::parse_parentheses_no_expr)),
        ),
        period_newline,
      ),
      |(tag, term)| ErlAst::new_generic_attr(&SourceLoc::from_input(input), tag, term),
    )(input)
  }

  /// Parses a `-module(atom).` attribute.
  /// Dash `-` and terminating `.` are matched outside by the caller.
  pub fn module_attr(input: &str) -> AstParserResult {
    map(
      delimited(
        match_dash_tag("module"),
        context(
          "the module name in a -module() attribute",
          cut(delimited(par_open, AtomParser::atom, par_close)),
        ),
        period_newline,
      ),
      |t| ErlAst::new_module_start_attr(&SourceLoc::from_input(input), t),
    )(input)
  }

  /// Parses a `fun/arity` atom with an integer.
  pub fn parse_funarity(input: &str) -> nom::IResult<&str, MFArity, ErlParserError> {
    map(tuple((AtomParser::atom, char('/'), parse_int)), |(name, _slash, erl_int)| {
      let arity = erl_int.as_usize().unwrap_or_default();
      MFArity::new_local_from_string(name, arity)
    })(input)
  }

  /// Parse a `fun/arity, ...` comma-separated list, at least 1 element long
  fn parse_square_funarity_list1(input: &str) -> nom::IResult<&str, Vec<MFArity>, ErlParserError> {
    delimited(
      square_open,
      separated_list1(comma, ws_before(Self::parse_funarity)),
      square_close,
    )(input)
  }

  /// Parses a list of mfarities: `( MFA/1, MFA/2, ... )` for export attr
  fn parse_export_mfa_list(input: &str) -> ParserResult<Vec<MFArity>> {
    delimited(par_open, ws_before(Self::parse_square_funarity_list1), par_close)(input)
  }

  /// Parses an `-export([fn/arity, ...]).` attribute.
  /// Dash `-` and trailing `.` are matched outside by the caller.
  pub fn export_attr(input: &str) -> AstParserResult {
    map(
      delimited(
        match_dash_tag("export"),
        context("list of exports in an -export() attribute", cut(Self::parse_export_mfa_list)),
        period_newline,
      ),
      |t| ErlAst::new_export_attr(&SourceLoc::from_input(input), t),
    )(input)
  }

  /// Parses an `-export_type([type/arity, ...]).` attribute.
  /// Dash `-` and trailing `.` are matched outside by the caller.
  pub fn export_type_attr(input: &str) -> AstParserResult {
    map(
      delimited(
        match_dash_tag("export_type"),
        context(
          "list of exports in an -export_type() attribute",
          cut(Self::parse_export_mfa_list),
        ),
        period_newline,
      ),
      |t| ErlAst::new_export_type_attr(&SourceLoc::from_input(input), t),
    )(input)
  }

  /// Parses an `-import(module [fn/arity, ...]).` attribute.
  /// Dash `-` and trailing `.` are matched outside by the caller.
  pub fn import_attr(input: &str) -> AstParserResult {
    map(
      delimited(
        match_dash_tag("import"),
        context(
          "list of imports in an -import() attribute",
          cut(delimited(
            par_open,
            separated_pair(AtomParser::atom, comma, Self::parse_square_funarity_list1),
            par_close,
          )),
        ),
        period_newline,
      ),
      |(mod_name, imports)| {
        ErlAst::new_import_attr(&SourceLoc::from_input(input), mod_name, imports)
      },
    )(input)
  }

  /// Parses a list of comma separated variables `(VAR1, VAR2, ...)`
  pub fn parse_parenthesized_list_of_vars(
    input: &str,
  ) -> nom::IResult<&str, Vec<String>, ErlParserError> {
    delimited(
      par_open,
      cut(separated_list0(comma, ErlTypeParser::parse_typevar_name)),
      par_close,
    )(input)
  }

  /// Parses a `-type IDENT(ARG, ...) :: TYPE.` attribute.
  /// Dash `-` and trailing `.` are matched outside by the caller.
  pub fn type_definition_attr(input: &str) -> AstParserResult {
    print_input("type_definition_attr", input);
    map(
      delimited(
        match_dash_tag("type"),
        tuple((
          AtomParser::atom,
          context(
            "type arguments in a -type() definition attribute",
            cut(Self::parse_parenthesized_list_of_vars),
          ),
          colon_colon,
          context("type in a -type() definition attribute", cut(ErlTypeParser::parse_type)),
        )),
        period_newline,
      ),
      |(type_name, type_args, _coloncolon, new_type)| {
        ErlAst::new_type_attr(&SourceLoc::from_input(input), type_name, type_args, new_type)
      },
    )(input)
  }

  /// Parses one field from the field list of `-record(atom(), { <FIELDS> } ).`.
  /// The field parser has a structure: `ATOM ( = EXPR ) ( :: TYPE )`
  fn record_definition_field(input: &str) -> ParserResult<RecordField> {
    map(
      tuple((
        AtomParser::atom,
        opt(preceded(equals_sign, ErlParser::parse_expr)),
        opt(preceded(colon_colon, ErlTypeParser::parse_type)),
      )),
      |(field_tag, opt_initializer, opt_type)| RecordField {
        field_tag,
        initializer: opt_initializer,
        type_ascription: opt_type,
      },
    )(input)
  }

  /// Parses inner contents of `-record( <INNER> ).`
  fn record_definition_inner(input: &str) -> AstParserResult {
    map(
      separated_pair(
        AtomParser::atom,
        comma,
        delimited(curly_open, separated_list0(comma, Self::record_definition_field), curly_close),
      ),
      |(atom, fields)| ErlAst::new_record_definition(&SourceLoc::from_input(input), atom, fields),
    )(input)
  }

  /// Parses a `-record(atom(), {field :: type()... }).` attribute.
  pub fn record_definition(input: &str) -> AstParserResult {
    delimited(
      match_dash_tag("record"),
      context(
        "record definition in a -record() attribute",
        cut(delimited(par_open, Self::record_definition_inner, par_close)),
      ),
      period_newline,
    )(input)
  }

  /// Any module attribute goes here
  pub fn attr(input: &str) -> AstParserResult {
    // print_input("attr", input);
    alt((
      Self::export_type_attr,
      Self::export_attr,
      Self::import_attr,
      Self::type_definition_attr,
      Self::module_attr,
      ErlTypeParser::fn_spec_attr,
      Self::record_definition,
      // Generic parser will try consume any `-IDENT(EXPR).`
      Self::parse_generic_attr,
    ))(input)
  }
}
