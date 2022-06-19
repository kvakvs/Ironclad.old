//! Use nom parser to parse a generic module attribute from a wall of text.
use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::defs::{ErlParserError, ParserResult};
use crate::erl_syntax::parsers::misc::{dash_atom, period_newline, tok, tok_atom, tok_integer};
use crate::erl_syntax::parsers::parse_expr::parse_expr;
use crate::erl_syntax::parsers::parse_record::parse_record_def;
use crate::erl_syntax::parsers::parse_type::ErlTypeParser;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::source_loc::SourceLoc;
use libironclad_util::mfarity::MFArity;
use nom::branch::alt;
use nom::combinator::{cut, map};
use nom::error::context;
use nom::multi::{separated_list0, separated_list1};
use nom::sequence::{delimited, pair, separated_pair, tuple};

/// Parse a `()` for a generic attribute `-<atom>().` and return empty `ErlAst`
fn parse_parentheses_no_expr(input: ParserInput) -> ParserResult<Option<AstNode>> {
  map(pair(tok(TokenType::ParOpen), tok(TokenType::ParClose)), |_| None)(input)
}

/// Parse a `( EXPR )` for a generic attribute `-<atom> ( EXPR ).`
fn parse_generic_attr_expr(input: ParserInput) -> ParserResult<Option<AstNode>> {
  map(
    delimited(
      tok(TokenType::ParOpen),
      context("an expression inside a custom -<name>() attribute", cut(parse_expr)),
      tok(TokenType::ParClose),
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
      tok(TokenType::Minus),
      pair(
        tok_atom,
        // Expr in parentheses
        alt((parse_parentheses_no_expr, parse_generic_attr_expr)),
      ),
      period_newline,
    ),
    |(tag, term)| AstNodeImpl::new_generic_attr(SourceLoc::new(&input), tag, term),
  )(input.clone())
}

/// Parses a generic `-TAG.` attribute, no parentheses, no expr.
pub(crate) fn parse_generic_attr_no_parentheses(input: ParserInput) -> ParserResult<AstNode> {
  map(delimited(tok(TokenType::Minus), tok_atom, period_newline), |tag| {
    AstNodeImpl::new_generic_attr(SourceLoc::new(&input), tag, None)
  })(input.clone())
}

/// Parses a `-module(atom).` attribute.
/// Dash `-` and terminating `.` are matched outside by the caller.
/// Will create error if the attribute does not parse (essentially a required attribute).
pub(crate) fn module_start_attr(input: ParserInput) -> ParserResult<String> {
  context(
    "expected -module() attribute",
    cut(delimited(
      dash_atom("module"),
      context(
        "the module name in a -module() attribute",
        cut(delimited(tok(TokenType::ParOpen), tok_atom, tok(TokenType::ParClose))),
      ),
      period_newline,
    )),
  )(input.clone())
}

/// Parses a `fun/arity` atom with an integer.
pub fn parse_funarity(input: ParserInput) -> nom::IResult<ParserInput, MFArity, ErlParserError> {
  map(
    tuple((tok_atom, tok(TokenType::Div), tok_integer)),
    |(name, _slash, erl_int)| {
      let arity = erl_int.as_usize().unwrap_or_default();
      MFArity::new_local_from_string(name, arity)
    },
  )(input)
}

/// Parse a `fun/arity, ...` comma-separated list, at least 1 element long
fn parse_square_funarity_list1(
  input: ParserInput,
) -> nom::IResult<ParserInput, Vec<MFArity>, ErlParserError> {
  delimited(
    tok(TokenType::SquareOpen),
    separated_list1(tok(TokenType::Comma), parse_funarity),
    tok(TokenType::SquareClose),
  )(input)
}

/// Parses a list of mfarities: `( MFA/1, MFA/2, ... )` for export attr
fn parse_export_mfa_list(input: ParserInput) -> ParserResult<Vec<MFArity>> {
  delimited(tok(TokenType::ParOpen), parse_square_funarity_list1, tok(TokenType::ParClose))(input)
}

/// Parses an `-export([fn/arity, ...]).` attribute.
/// Dash `-` and trailing `.` are matched outside by the caller.
pub(crate) fn export_attr(input: ParserInput) -> ParserResult<AstNode> {
  map(
    delimited(
      dash_atom("export"),
      context("list of exports in an -export() attribute", cut(parse_export_mfa_list)),
      period_newline,
    ),
    |t| AstNodeImpl::new_export_attr(SourceLoc::new(&input), t),
  )(input.clone())
}

/// Parses an `-export_type([type/arity, ...]).` attribute.
/// Dash `-` and trailing `.` are matched outside by the caller.
pub(crate) fn export_type_attr(input: ParserInput) -> ParserResult<AstNode> {
  map(
    delimited(
      dash_atom("export_type"),
      context("list of exports in an -export_type() attribute", cut(parse_export_mfa_list)),
      period_newline,
    ),
    |t| AstNodeImpl::new_export_type_attr(SourceLoc::new(&input), t),
  )(input.clone())
}

/// Parses an `-import(module [fn/arity, ...]).` attribute.
/// Dash `-` and trailing `.` are matched outside by the caller.
pub(crate) fn import_attr(input: ParserInput) -> ParserResult<AstNode> {
  map(
    delimited(
      dash_atom("import"),
      context(
        "list of imports in an -import() attribute",
        cut(delimited(
          tok(TokenType::ParOpen),
          separated_pair(tok_atom, tok(TokenType::Comma), parse_square_funarity_list1),
          tok(TokenType::ParClose),
        )),
      ),
      period_newline,
    ),
    |(mod_name, imports)| AstNodeImpl::new_import_attr(SourceLoc::new(&input), mod_name, imports),
  )(input.clone())
}

/// Parses a list of comma separated variables `(VAR1, VAR2, ...)`
pub(crate) fn parse_parenthesized_list_of_vars(
  input: ParserInput,
) -> nom::IResult<ParserInput, Vec<String>, ErlParserError> {
  delimited(
    tok(TokenType::ParOpen),
    cut(separated_list0(tok(TokenType::Comma), ErlTypeParser::parse_typevar_name)),
    tok(TokenType::ParClose),
  )(input)
}

/// Parses a `-type IDENT(ARG, ...) :: TYPE.` attribute.
/// Dash `-` and trailing `.` are matched outside by the caller.
pub fn type_definition_attr(input: ParserInput) -> ParserResult<AstNode> {
  // print_input("type_definition_attr", input);
  map(
    delimited(
      dash_atom("type"),
      tuple((
        tok_atom,
        context(
          "type arguments in a -type() definition attribute",
          cut(parse_parenthesized_list_of_vars),
        ),
        tok(TokenType::ColonColon),
        context("type in a -type() definition attribute", cut(ErlTypeParser::parse_type)),
      )),
      period_newline,
    ),
    |(type_name, type_args, _coloncolon, new_type)| {
      AstNodeImpl::new_type_attr(SourceLoc::new(&input), type_name, type_args, new_type)
    },
  )(input.clone())
}

/// Any module attribute goes here
pub(crate) fn parse_module_attr(input: ParserInput) -> ParserResult<AstNode> {
  // print_input("attr", input);
  alt((
    parse_record_def,
    export_type_attr,
    export_attr,
    import_attr,
    type_definition_attr,
    ErlTypeParser::fn_spec_attr,
    // Generic parser will try consume any `-IDENT(EXPR).`
    parse_generic_attr,
    parse_generic_attr_no_parentheses,
  ))(input)
}
