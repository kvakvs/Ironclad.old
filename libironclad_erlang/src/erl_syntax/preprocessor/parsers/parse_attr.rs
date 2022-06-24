//! Use nom parser to stage_parse a generic module attribute from a wall of text.
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{
  dash_atom, period_eol_eof, tok, tok_atom, tok_comma, tok_forward_slash, tok_integer, tok_minus,
  tok_par_close, tok_par_open, tok_square_close, tok_square_open,
};
use crate::erl_syntax::parsers::parse_expr::parse_expr;
use crate::erl_syntax::parsers::parse_type::ErlTypeParser;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::preprocessor::parsers::parse_record::parse_record_def;
use crate::erl_syntax::preprocessor::pp_node::pp_impl::PreprocessorNodeImpl;
use crate::erl_syntax::preprocessor::pp_node::PreprocessorNode;
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::source_loc::SourceLoc;
use libironclad_util::mfarity::MFArity;
use nom::branch::alt;
use nom::combinator::{cut, map};
use nom::error::context;
use nom::multi::{separated_list0, separated_list1};
use nom::sequence::{delimited, pair, separated_pair, tuple};

/// Parse a `()` for a generic attribute `-<atom>().` and return empty `ErlAst`
fn attr_body_empty_parens(input: ParserInput) -> ParserResult<Option<AstNode>> {
  map(pair(tok_par_open, tok_par_close), |_| None)(input)
}

/// Parse a `( EXPR )` for a generic attribute `-<atom> ( EXPR ).`
fn attr_body_expr_in_parens(input: ParserInput) -> ParserResult<Option<AstNode>> {
  map(
    delimited(
      tok_par_open,
      context("an expression inside a custom -<name>() attribute", cut(parse_expr)),
      tok_par_close,
    ),
    Option::Some,
  )(input)
}

/// Parses a generic `-TAG(TERM)."` attribute.
/// Given a string, try and consume a generic attribute line starting with `-ident` and ending with
/// a `"." NEWLINE`.
pub fn parse_generic_attr(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(
    delimited(
      tok_minus,
      pair(
        tok_atom,
        // Expr in parentheses or nothing
        alt((attr_body_empty_parens, attr_body_expr_in_parens)),
      ),
      period_eol_eof,
    ),
    |(tag, term)| PreprocessorNodeImpl::new_generic_attr(SourceLoc::new(&input), tag, term),
  )(input.clone())
}

/// Parses a generic `-TAG.` attribute, no parentheses, no expr.
pub(crate) fn parse_generic_attr_no_parentheses(
  input: ParserInput,
) -> ParserResult<PreprocessorNode> {
  map(delimited(tok_minus, tok_atom, period_eol_eof), |tag| {
    PreprocessorNodeImpl::new_generic_attr(SourceLoc::new(&input), tag, None)
  })(input.clone())
}

/// Parses a `fun/arity` atom with an integer.
pub fn parse_funarity(input: ParserInput) -> ParserResult<MFArity> {
  map(tuple((tok_atom, tok_forward_slash, tok_integer)), |(name, _slash, erl_int)| {
    let arity = erl_int.as_usize().unwrap_or_default();
    MFArity::new_local_from_string(name, arity)
  })(input)
}

/// Parse a `fun/arity, ...` comma-separated list, at least 1 element long
fn parse_square_funarity_list1(input: ParserInput) -> ParserResult<Vec<MFArity>> {
  delimited(tok_square_open, separated_list1(tok_comma, parse_funarity), tok_square_close)(input)
}

/// Parses a list of mfarities: `( MFA/1, MFA/2, ... )` for export attr
fn parse_export_mfa_list(input: ParserInput) -> ParserResult<Vec<MFArity>> {
  delimited(tok_par_open, parse_square_funarity_list1, tok_par_close)(input)
}

/// Parses an `-export([fn/arity, ...]).` attribute.
/// Dash `-` and trailing `.` are matched outside by the caller.
pub(crate) fn export_attr(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(
    delimited(
      |i1| dash_atom(i1, "export"),
      context("list of exports in an -export() attribute", cut(parse_export_mfa_list)),
      period_eol_eof,
    ),
    |t| PreprocessorNodeImpl::new_export_attr(SourceLoc::new(&input), t),
  )(input.clone())
}

/// Parses an `-export_type([type/arity, ...]).` attribute.
/// Dash `-` and trailing `.` are matched outside by the caller.
pub(crate) fn export_type_attr(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(
    delimited(
      |i1| dash_atom(i1, "export_type"),
      context("list of exports in an -export_type() attribute", cut(parse_export_mfa_list)),
      period_eol_eof,
    ),
    |t| PreprocessorNodeImpl::new_export_type_attr(SourceLoc::new(&input), t),
  )(input.clone())
}

/// Parses an `-import(module [fn/arity, ...]).` attribute.
/// Dash `-` and trailing `.` are matched outside by the caller.
pub(crate) fn import_attr(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(
    delimited(
      |i1| dash_atom(i1, "import"),
      context(
        "list of imports in an -import() attribute",
        cut(delimited(
          tok_par_open,
          separated_pair(tok_atom, tok_comma, parse_square_funarity_list1),
          tok_par_close,
        )),
      ),
      period_eol_eof,
    ),
    |(mod_name, imports)| {
      PreprocessorNodeImpl::new_import_attr(SourceLoc::new(&input), mod_name, imports)
    },
  )(input.clone())
}

/// Parses a list of comma separated variables `(VAR1, VAR2, ...)`
pub(crate) fn parse_parenthesized_list_of_vars(input: ParserInput) -> ParserResult<Vec<String>> {
  delimited(
    tok_par_open,
    cut(separated_list0(tok_comma, ErlTypeParser::parse_typevar_name)),
    tok_par_close,
  )(input)
}

/// Parses a `-type IDENT(ARG, ...) :: TYPE.` attribute.
/// Dash `-` and trailing `.` are matched outside by the caller.
pub fn type_definition_attr(input: ParserInput) -> ParserResult<PreprocessorNode> {
  // print_input("type_definition_attr", input);
  map(
    delimited(
      |i1| dash_atom(i1, "type"),
      tuple((
        tok_atom,
        context(
          "type arguments in a -type() definition attribute",
          cut(parse_parenthesized_list_of_vars),
        ),
        tok(TokenType::ColonColon),
        context("type in a -type() definition attribute", cut(ErlTypeParser::parse_type)),
      )),
      period_eol_eof,
    ),
    |(type_name, type_args, _coloncolon, new_type)| {
      PreprocessorNodeImpl::new_type_attr(SourceLoc::new(&input), type_name, type_args, new_type)
    },
  )(input.clone())
}

/// Any module attribute goes here
pub(crate) fn parse_any_module_attr(input: ParserInput) -> ParserResult<PreprocessorNode> {
  alt((
    parse_record_def,
    export_type_attr,
    export_attr,
    import_attr,
    type_definition_attr,
    ErlTypeParser::parse_fn_spec,
    // Generic parser will try consume any `-IDENT(EXPR).`
    parse_generic_attr,
    parse_generic_attr_no_parentheses,
  ))(input)
}
