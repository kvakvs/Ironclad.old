//! Use nom parser to parse a generic module attribute from a wall of text.
use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::defs::ParserInput;
use crate::erl_syntax::parsers::defs::{ErlParserError, ParserResult};
use crate::erl_syntax::parsers::misc::{
  colon_colon_tag, comma_tag, match_dash_tag, par_close_tag, par_open_tag, parse_int,
  period_newline_tag, square_close_tag, square_open_tag, ws_before,
};
use crate::erl_syntax::parsers::parse_atom::parse_atom;
use crate::erl_syntax::parsers::parse_record::parse_record_def;
use crate::erl_syntax::parsers::parse_type::ErlTypeParser;
use crate::erl_syntax::parsers::ErlParser;
use libironclad_util::mfarity::MFArity;
use nom::branch::alt;
use nom::combinator::{cut, map};
use nom::multi::{separated_list0, separated_list1};
use nom::sequence::{delimited, pair, separated_pair, tuple};
use nom::{character::complete::char, error::context};

/// Parse a `()` for a generic attribute `-<atom>().` and return empty `ErlAst`
fn parse_parentheses_no_expr(input: ParserInput) -> ParserResult<Option<AstNode>> {
  map(pair(par_open_tag, par_close_tag), |_| None)(input)
}

/// Parse a `( EXPR )` for a generic attribute `-<atom> ( EXPR ).`
fn parse_generic_attr_expr(input: ParserInput) -> ParserResult<Option<AstNode>> {
  map(
    delimited(
      par_open_tag,
      context("an expression inside a custom -<name>() attribute", cut(ErlParser::parse_expr)),
      par_close_tag,
    ),
    Option::Some,
  )(input)
}

/// Parses a generic `-TAG(TERM)."` attribute.
/// Given a string, try and consume a generic attribute line starting with `-ident` and ending with
/// a `"." NEWLINE`.
pub fn parse_generic_attr(input: ParserInput) -> ParserResult<AstNode> {
  map(
    delimited(
      ws_before(char('-')),
      pair(
        parse_atom,
        // Expr in parentheses
        alt((parse_generic_attr_expr, parse_parentheses_no_expr)),
      ),
      period_newline_tag,
    ),
    |(tag, term)| AstNodeImpl::new_generic_attr(input.loc(), tag, term),
  )(input.clone())
}

/// Parses a `-module(atom).` attribute.
/// Dash `-` and terminating `.` are matched outside by the caller.
pub fn module_attr(input: ParserInput) -> ParserResult<AstNode> {
  map(
    delimited(
      match_dash_tag("module".into()),
      context(
        "the module name in a -module() attribute",
        cut(delimited(par_open_tag, parse_atom, par_close_tag)),
      ),
      period_newline_tag,
    ),
    |t| AstNodeImpl::new_module_start_attr(input.loc(), t),
  )(input.clone())
}

/// Parses a `fun/arity` atom with an integer.
pub fn parse_funarity(input: ParserInput) -> nom::IResult<ParserInput, MFArity, ErlParserError> {
  map(tuple((parse_atom, char('/'), parse_int)), |(name, _slash, erl_int)| {
    let arity = erl_int.as_usize().unwrap_or_default();
    MFArity::new_local_from_string(name, arity)
  })(input)
}

/// Parse a `fun/arity, ...` comma-separated list, at least 1 element long
fn parse_square_funarity_list1(
  input: ParserInput,
) -> nom::IResult<ParserInput, Vec<MFArity>, ErlParserError> {
  delimited(
    square_open_tag,
    separated_list1(comma_tag, ws_before(parse_funarity)),
    square_close_tag,
  )(input)
}

/// Parses a list of mfarities: `( MFA/1, MFA/2, ... )` for export attr
fn parse_export_mfa_list(input: ParserInput) -> ParserResult<Vec<MFArity>> {
  delimited(par_open_tag, ws_before(parse_square_funarity_list1), par_close_tag)(input)
}

/// Parses an `-export([fn/arity, ...]).` attribute.
/// Dash `-` and trailing `.` are matched outside by the caller.
pub fn export_attr(input: ParserInput) -> ParserResult<AstNode> {
  map(
    delimited(
      match_dash_tag("export".into()),
      context("list of exports in an -export() attribute", cut(parse_export_mfa_list)),
      period_newline_tag,
    ),
    |t| AstNodeImpl::new_export_attr(input.loc(), t),
  )(input.clone())
}

/// Parses an `-export_type([type/arity, ...]).` attribute.
/// Dash `-` and trailing `.` are matched outside by the caller.
pub fn export_type_attr(input: ParserInput) -> ParserResult<AstNode> {
  map(
    delimited(
      match_dash_tag("export_type".into()),
      context("list of exports in an -export_type() attribute", cut(parse_export_mfa_list)),
      period_newline_tag,
    ),
    |t| AstNodeImpl::new_export_type_attr(input.loc(), t),
  )(input.clone())
}

/// Parses an `-import(module [fn/arity, ...]).` attribute.
/// Dash `-` and trailing `.` are matched outside by the caller.
pub fn import_attr(input: ParserInput) -> ParserResult<AstNode> {
  map(
    delimited(
      match_dash_tag("import".into()),
      context(
        "list of imports in an -import() attribute",
        cut(delimited(
          par_open_tag,
          separated_pair(parse_atom, comma_tag, parse_square_funarity_list1),
          par_close_tag,
        )),
      ),
      period_newline_tag,
    ),
    |(mod_name, imports)| AstNodeImpl::new_import_attr(input.loc(), mod_name, imports),
  )(input.clone())
}

/// Parses a list of comma separated variables `(VAR1, VAR2, ...)`
pub fn parse_parenthesized_list_of_vars(
  input: ParserInput,
) -> nom::IResult<ParserInput, Vec<String>, ErlParserError> {
  delimited(
    par_open_tag,
    cut(separated_list0(comma_tag, ErlTypeParser::parse_typevar_name)),
    par_close_tag,
  )(input)
}

/// Parses a `-type IDENT(ARG, ...) :: TYPE.` attribute.
/// Dash `-` and trailing `.` are matched outside by the caller.
pub fn type_definition_attr(input: ParserInput) -> ParserResult<AstNode> {
  // print_input("type_definition_attr", input);
  map(
    delimited(
      match_dash_tag("type".into()),
      tuple((
        parse_atom,
        context(
          "type arguments in a -type() definition attribute",
          cut(parse_parenthesized_list_of_vars),
        ),
        colon_colon_tag,
        context("type in a -type() definition attribute", cut(ErlTypeParser::parse_type)),
      )),
      period_newline_tag,
    ),
    |(type_name, type_args, _coloncolon, new_type)| {
      AstNodeImpl::new_type_attr(input.loc(), type_name, type_args, new_type)
    },
  )(input.clone())
}

/// Any module attribute goes here
pub fn parse_module_attr(input: ParserInput) -> ParserResult<AstNode> {
  // print_input("attr", input);
  alt((
    export_type_attr,
    export_attr,
    import_attr,
    type_definition_attr,
    module_attr,
    ErlTypeParser::fn_spec_attr,
    parse_record_def,
    // Generic parser will try consume any `-IDENT(EXPR).`
    parse_generic_attr,
  ))(input)
}
