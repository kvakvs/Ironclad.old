//! Quick scan through a source file, split it using preprocessor directives as a divider

use crate::nom_parser::pp_parse_types::{
  PpAstParserResult, PpParserResult, PpStringParserResult, PreprocessorParser,
  StrSliceParserResult, VecPpAstParserResult,
};
use crate::syntax_tree::pp_ast::PpAst;
use libironclad_erlang::syntax_tree::nom_parse::misc::{
  comma, newline_or_eof, par_close, par_open, parse_ident, parse_line_comment, period, ws,
  ws_before, ws_before_mut,
};
use libironclad_erlang::syntax_tree::nom_parse::parse_str::StringParser;
use nom::branch::alt;
use nom::character::complete::anychar;
use nom::combinator::{map, opt, recognize, verify};
use nom::multi::{many1, many_till, separated_list0};
use nom::sequence::{delimited, pair, preceded, tuple};
use nom::{
  bytes::complete::tag,
  character,
  character::complete::{alphanumeric1, char},
  error::context,
};

pub mod pp_parse_if;
pub mod pp_parse_types;

impl PreprocessorParser {
  // /// Parse a `Var1, Var2, ...` into a list
  // fn parse_comma_sep_varnames(input: &str) -> PpParserResult<Vec<String>> {
  //   separated_list0(comma, parse_varname)(input)
  // }

  /// Parse a `ident1, ident2, ...` into a list
  fn parse_comma_sep_idents(input: &str) -> PpParserResult<Vec<String>> {
    separated_list0(comma, parse_ident)(input)
  }

  fn parenthesis_dot_newline(input: &str) -> PpParserResult<&str> {
    recognize(tuple((par_close, period, newline_or_eof)))(input)
  }

  /// Parses inner part of a `-define(IDENT)` variant
  fn parse_define_ident_only(input: &str) -> PpAstParserResult {
    map(ws_before(parse_ident), PpAst::new_define_name_only)(input)
  }

  /// Parses inner part of a `-define(IDENT(ARGS), BODY).` or without args `-define(IDENT, BODY).`
  /// will consume end delimiter `").\n"`
  fn parse_define_ident_args_with_terminator(input: &str) -> PpAstParserResult {
    map(
      tuple((
        ws_before(parse_ident),
        // Optional args
        opt(delimited(par_open, Self::parse_comma_sep_idents, par_close)),
        // Followed by a body
        opt(delimited(
          comma,
          many_till(anychar, Self::parenthesis_dot_newline),
          Self::parenthesis_dot_newline,
        )),
      )),
      |(name, args, body)| {
        PpAst::new_define(
          name,
          args,
          body.map(|(chars, _term)| chars.into_iter().collect::<String>()),
        )
      },
    )(input)
  }

  /// Parse a `-define(NAME)` or `-define(NAME, VALUE)` or `-define(NAME(ARGS,...), VALUE)`
  pub fn parse_define(input: &str) -> PpAstParserResult {
    delimited(
      Self::match_dash_tag("define"),
      alt((
        delimited(par_open, Self::parse_define_ident_only, par_close),
        // `parse_define_ident_args_with_terminator` will consume end delimiter
        preceded(par_open, Self::parse_define_ident_args_with_terminator),
      )),
      Self::dot_newline,
    )(input)
  }

  /// Parse an identifier, starting with lowercase and also can be containing numbers and underscoress
  fn parse_macro_ident(input: &str) -> PpStringParserResult {
    map(
      recognize(pair(
        verify(character::complete::anychar, |c: &char| c.is_alphabetic() || *c == '_'),
        many1(alt((alphanumeric1, tag("_")))),
      )),
      |result: &str| result.to_string(),
    )(input)
  }

  /// Parse a `-undef(IDENT)`
  fn parse_undef(input: &str) -> PpAstParserResult {
    map(
      delimited(
        Self::match_dash_tag("undef"),
        tuple((par_open, ws_before(Self::parse_macro_ident), par_close)),
        Self::dot_newline,
      ),
      |(_open, ident, _close)| PpAst::new_undef(ident),
    )(input)
  }

  /// Parse a `-include(STRING)`
  fn parse_include(input: &str) -> PpAstParserResult {
    map(
      preceded(
        Self::match_dash_tag("include"),
        tuple((par_open, ws_before(StringParser::parse_string), par_close)),
      ),
      |(_open, s, _close)| PpAst::new_include(s),
    )(input)
  }

  /// Parse a `-include_lib(STRING)`
  fn parse_include_lib(input: &str) -> PpAstParserResult {
    map(
      preceded(
        Self::match_dash_tag("include_lib"),
        tuple((par_open, ws_before(StringParser::parse_string), par_close)),
      ),
      |(_open, s, _close)| PpAst::new_include_lib(s),
    )(input)
  }

  /// Returns a new parser which recognizes a `<spaces> "-" <spaces> <tag>` and returns it as
  /// a `&str` slice
  fn match_dash_tag<'a, ErrType: 'a + nom::error::ParseError<&'a str>>(
    tag_str: &'static str,
  ) -> impl FnMut(&'a str) -> nom::IResult<&'a str, &'a str, ErrType> {
    recognize(pair(ws_before(char('-')), ws_before(tag(tag_str))))
  }

  /// Recognizes end of a directive: `"." <newline>`
  fn dot_newline(input: &str) -> StrSliceParserResult {
    recognize(pair(period, newline_or_eof))(input)
  }

  fn parse_preproc_directive(input: &str) -> PpAstParserResult {
    ws_before_mut(alt((
      // -define is special, it needs closing ).\n to consume the content
      context("'-define' directive", Self::parse_define),
      context("'-undef' directive", Self::parse_undef),
      // temporary nodes used by parse_if_block
      context("'-endif' directive", Self::parse_endif_temporary),
      context("'-elif' directive", Self::parse_elif_temporary),
      context("'-else' directive", Self::parse_else_temporary),
      context("'-ifdef' directive", Self::parse_ifdef_temporary),
      context("'-ifndef' directive", Self::parse_ifndef_temporary),
      context("'-if' directive", Self::parse_if_block), // if must go after longer words ifdef and ifndef
      // Self::parse_error,
      // Self::parse_warning,
      context("'-include_lib' directive", Self::parse_include_lib),
      context("'-include' directive", Self::parse_include),
    )))(input)
  }

  /// Parse full lines till a line which looks like a preprocessor directive is found
  fn consume_one_line_of_text(input: &str) -> PpAstParserResult {
    map(
      verify(
        ws(nom::bytes::complete::take_till(|c| c == '\n' || c == '\r')),
        |text: &str| !text.is_empty(), //&& !text.starts_with('-'),
      ),
      PpAst::new_text,
    )(input)
  }

  /// Parses either a preprocessor directive or block, or consumes one line of text
  fn parse_fragment(input: &str) -> PpAstParserResult {
    alt((
      Self::parse_preproc_directive,
      Self::consume_one_line_of_text,
      // A final comment in file is not visible to consume_text
      map(parse_line_comment, |_| PpAst::new_text("")),
    ))(input)
  }

  /// Split input into AST nodes for preprocessor directives and any irrelevant text in between
  pub fn parse_fragments_collection(input: &str) -> VecPpAstParserResult {
    // Followed by 1 or more directive or another text fragment
    many1(Self::parse_fragment)(input)
  }

  /// Parses file contents into mix of preprocessor directives and text fragments.
  /// Comments are eliminated.
  pub fn parse_module(input: &str) -> PpAstParserResult {
    let (input, fragments) = Self::parse_fragments_collection(input)?;
    Ok((input, PpAst::new_file(fragments)))
  }
}
