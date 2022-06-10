//! Groups type definitions shared by all preprocessor parse modules
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::defs::{ParserInput, ParserResult};
use crate::erl_syntax::parsers::misc::{
  comma_tag, match_dash_tag, newline_or_eof, par_close_tag, par_open_tag, period_newline_tag,
  period_tag, ws_before, ws_before_mut,
};
use crate::erl_syntax::parsers::parse_strings::str_literal::parse_doublequot_string;
use crate::erl_syntax::preprocessor::ast::PreprocessorNodeType;
use crate::erl_syntax::preprocessor::parsers::def_undef::{define_directive, undef_directive};
use crate::erl_syntax::preprocessor::parsers::r#if::{
  elif_temporary_directive, else_temporary_directive, endif_temporary_directive,
  ifdef_temporary_directive, ifndef_temporary_directive, parse_if_block,
};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alphanumeric1, anychar};
use nom::combinator::{cut, map, recognize, verify};
use nom::error::context;
use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, pair, tuple};

/// Parse a `Macroident1, Macroident2, ...` into a list
pub(crate) fn comma_sep_macro_idents(input: ParserInput) -> ParserResult<Vec<String>> {
  separated_list0(comma_tag, macro_ident)(input)
}

pub(crate) fn parenthesis_dot_newline(input: ParserInput) -> ParserResult<ParserInput> {
  recognize(tuple((par_close_tag, period_tag, newline_or_eof)))(input)
}

/// Parse an identifier, starting with a letter and also can be containing numbers and underscoress
pub(crate) fn macro_ident(input: ParserInput) -> ParserResult<String> {
  map(
    ws_before_mut(recognize(pair(
      verify(anychar, |c: &char| c.is_alphabetic() || *c == '_'),
      many0(alt((alphanumeric1, tag("_".into())))),
    ))),
    |pi| pi.to_string(),
  )(input)
}

/// Parse a `-include(STRING)`
fn include_directive(input: ParserInput) -> ParserResult<AstNode> {
  map(
    delimited(
      match_dash_tag("include".into()),
      delimited(
        par_open_tag,
        context("include path for -include() directive", cut(parse_doublequot_string)),
        par_close_tag,
      ),
      period_newline_tag,
    ),
    |t| PreprocessorNodeType::new_include(input.loc(), t),
  )(input.clone())
}

/// Parse a `-include_lib(STRING)`
fn include_lib_directive(input: ParserInput) -> ParserResult<AstNode> {
  map(
    delimited(
      match_dash_tag("include_lib".into()),
      delimited(par_open_tag, ws_before(parse_doublequot_string), par_close_tag),
      period_newline_tag,
    ),
    |t| PreprocessorNodeType::new_include_lib(input.loc(), t),
  )(input.clone())
}

/// Parse a `-error(STRING)`
fn error_directive(input: ParserInput) -> ParserResult<AstNode> {
  map(
    delimited(
      match_dash_tag("error".into()),
      delimited(
        par_open_tag,
        context("string literal for -error() directive", cut(ws_before(parse_doublequot_string))),
        par_close_tag,
      ),
      period_newline_tag,
    ),
    |t| PreprocessorNodeType::new_error(input.loc(), t),
  )(input.clone())
}

/// Parse a `-warning(STRING)`
fn warning_directive(input: ParserInput) -> ParserResult<AstNode> {
  map(
    delimited(
      match_dash_tag("warning".into()),
      delimited(
        par_open_tag,
        context("string literal for -error() directive", cut(ws_before(parse_doublequot_string))),
        par_close_tag,
      ),
      period_newline_tag,
    ),
    |t| PreprocessorNodeType::new_warning(input.loc(), t),
  )(input.clone())
}

/// Parse one of supported preprocessor directives
pub(crate) fn parse_preproc_directive(input: ParserInput) -> ParserResult<AstNode> {
  ws_before_mut(alt((
    // -define is special, it needs closing ).\n to consume the content
    context("'-define()' directive", define_directive),
    context("'-undef()' directive", undef_directive),
    // temporary nodes used by parse_if_block
    context("'-endif()' directive", endif_temporary_directive),
    context("'-elif()' directive", elif_temporary_directive),
    context("'-else()' directive", else_temporary_directive),
    context("'-ifdef()' directive", ifdef_temporary_directive),
    context("'-ifndef()' directive", ifndef_temporary_directive),
    context("'-if()' directive", parse_if_block), // if must go after longer words ifdef and ifndef
    context("'-warning()' directive", warning_directive),
    context("'-error()' directive", error_directive),
    context("'-include_lib()' directive", include_lib_directive),
    context("'-include()' directive", include_directive),
  )))(input)
}
