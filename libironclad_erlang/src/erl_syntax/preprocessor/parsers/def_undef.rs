//! Parse helpers for `-define`/`-undef` preprocessor

use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::defs::{ParserInput, ParserResult};
use crate::erl_syntax::parsers::misc::{
  comma_tag, match_dash_tag, par_close_tag, par_open_tag, period_newline_tag, ws_before_mut,
};
use crate::erl_syntax::preprocessor::ast::PreprocessorNodeType;
use crate::erl_syntax::preprocessor::parsers::preprocessor::{
  comma_sep_macro_idents, macro_ident, parenthesis_dot_newline,
};
use nom::branch::alt;
use nom::character::complete::anychar;
use nom::combinator::{map, opt};
use nom::error::context;
use nom::multi::many_till;
use nom::sequence::{delimited, preceded, tuple};

/// Parses inner part of a `-define(IDENT)` variant
fn define_no_args_no_body(input: ParserInput) -> ParserResult<AstNode> {
  map(macro_ident, |name: String| {
    PreprocessorNodeType::new_define_name_only(input.loc(), name)
  })(input.clone())
}

/// Parses inner part of a `-define(IDENT(ARGS), BODY).` or without args `-define(IDENT, BODY).`
/// will consume end delimiter `").\n"`
fn define_with_args_body_and_terminator(input: ParserInput) -> ParserResult<AstNode> {
  map(
    tuple((
      // Macro name
      macro_ident,
      // Optional (ARG1, ARG2, ...) with trailing comma
      opt(delimited(par_open_tag, comma_sep_macro_idents, par_close_tag)),
      comma_tag,
      // Followed by a body
      ws_before_mut(many_till(anychar, parenthesis_dot_newline)),
    )),
    |(name, args, _comma, (body, _terminator))| {
      PreprocessorNodeType::new_define(
        input.loc(),
        name,
        args.unwrap_or_default(),
        body.into_iter().collect::<String>(),
      )
    },
  )(input.clone())
}

/// Parse a `-define(NAME)` or `-define(NAME, VALUE)` or `-define(NAME(ARGS,...), VALUE)`
pub(crate) fn define_directive(input: ParserInput) -> ParserResult<AstNode> {
  preceded(
    match_dash_tag("define".into()),
    alt((
      context(
        "-define directive with no args and no body",
        delimited(par_open_tag, define_no_args_no_body, parenthesis_dot_newline),
      ),
      // `define_with_args_body_and_terminator` will consume end delimiter
      context(
        "-define directive with optional args and body",
        preceded(par_open_tag, define_with_args_body_and_terminator),
      ),
    )),
  )(input)
}

/// Parse a `-undef(IDENT)`
pub(crate) fn undef_directive(input: ParserInput) -> ParserResult<AstNode> {
  map(
    delimited(
      match_dash_tag("undef".into()),
      delimited(par_open_tag, macro_ident, par_close_tag),
      period_newline_tag,
    ),
    |ident: String| PreprocessorNodeType::new_undef(input.loc(), ident),
  )(input.clone())
}
