//! Parse helpers for `-define`/`-undef` preprocessor

use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::defs::{ParserInput, ParserResult};
use crate::erl_syntax::parsers::misc::{
  comma, match_dash_tag, par_close, par_open, period_newline, ws_before,
};
use crate::erl_syntax::preprocessor::ast::PreprocessorNodeType;
use crate::erl_syntax::preprocessor::parsers::preprocessor_parser::PreprocessorParser;
use nom::branch::alt;
use nom::character::complete::anychar;
use nom::combinator::{map, opt};
use nom::error::context;
use nom::multi::many_till;
use nom::sequence::{delimited, preceded, tuple};

impl PreprocessorParser {
  /// Parses inner part of a `-define(IDENT)` variant
  fn define_no_args_no_body(input: ParserInput) -> ParserResult<AstNode> {
    map(Self::macro_ident, |name| {
      PreprocessorNodeType::new_define_name_only(input.loc(), name.to_string())
    })(input.clone())
  }

  /// Parses inner part of a `-define(IDENT(ARGS), BODY).` or without args `-define(IDENT, BODY).`
  /// will consume end delimiter `").\n"`
  fn define_with_args_body_and_terminator(input: ParserInput) -> ParserResult<AstNode> {
    map(
      tuple((
        // Macro name
        Self::macro_ident,
        // Optional (ARG1, ARG2, ...) with trailing comma
        opt(delimited(par_open, Self::comma_sep_macro_idents, par_close)),
        comma,
        // Followed by a body
        many_till(anychar, Self::parenthesis_dot_newline),
      )),
      |(name, args, _comma, (body, _terminator))| {
        PreprocessorNodeType::new_define(
          input.loc(),
          name.to_string(),
          args.unwrap_or_default(),
          body.into_iter().collect::<String>(),
        )
      },
    )(input.clone())
  }

  /// Parse a `-define(NAME)` or `-define(NAME, VALUE)` or `-define(NAME(ARGS,...), VALUE)`
  pub fn define_directive(input: ParserInput) -> ParserResult<AstNode> {
    preceded(
      match_dash_tag("define".into()),
      alt((
        context(
          "-define directive with no args and no body",
          delimited(par_open, Self::define_no_args_no_body, Self::parenthesis_dot_newline),
        ),
        // `define_with_args_body_and_terminator` will consume end delimiter
        context(
          "-define directive with optional args and body",
          preceded(par_open, Self::define_with_args_body_and_terminator),
        ),
      )),
    )(input)
  }

  /// Parse a `-undef(IDENT)`
  pub fn undef_directive(input: ParserInput) -> ParserResult<AstNode> {
    map(
      delimited(
        match_dash_tag("undef".into()),
        tuple((par_open, ws_before(Self::macro_ident), par_close)),
        period_newline,
      ),
      |(_open, ident, _close)| PreprocessorNodeType::new_undef(input.loc(), ident.to_string()),
    )(input.clone())
  }
}
