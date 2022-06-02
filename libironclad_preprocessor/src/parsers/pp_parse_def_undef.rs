//! Parse helpers for `-define`/`-undef` preprocessor

use crate::parsers::pp_parse_types::{PpAstParserResult, PreprocessorParser};
use crate::preprocessor_syntax::pp_ast::PpAst;
use crate::preprocessor_syntax::pp_macro_string::MacroString;
use libironclad_erlang::erl_syntax::parsers::misc::{
  comma, match_dash_tag, par_close, par_open, period_newline, ws_before,
};
use libironclad_error::source_loc::SourceLoc;
use nom::branch::alt;
use nom::character::complete::anychar;
use nom::combinator::{map, opt};
use nom::error::context;
use nom::multi::many_till;
use nom::sequence::{delimited, preceded, tuple};

impl PreprocessorParser {
  /// Parses inner part of a `-define(IDENT)` variant
  fn define_no_args_no_body(input: &str) -> PpAstParserResult {
    map(Self::macro_ident, |name| {
      PpAst::new_define_name_only(&SourceLoc::from_input(input), name)
    })(input)
  }

  /// Parses inner part of a `-define(IDENT(ARGS), BODY).` or without args `-define(IDENT, BODY).`
  /// will consume end delimiter `").\n"`
  fn define_with_args_body_and_terminator(input: &str) -> PpAstParserResult {
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
        PpAst::new_define(
          &SourceLoc::from_input(input),
          name,
          args.unwrap_or_default(),
          MacroString::new_string(body.into_iter().collect::<String>()),
        )
      },
    )(input)
  }

  /// Parse a `-define(NAME)` or `-define(NAME, VALUE)` or `-define(NAME(ARGS,...), VALUE)`
  pub fn define_directive(input: &str) -> PpAstParserResult {
    preceded(
      match_dash_tag("define"),
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
  pub fn undef_directive(input: &str) -> PpAstParserResult {
    map(
      delimited(
        match_dash_tag("undef"),
        tuple((par_open, ws_before(Self::macro_ident), par_close)),
        period_newline,
      ),
      |(_open, ident, _close)| PpAst::new_undef(&SourceLoc::from_input(input), ident),
    )(input)
  }
}
