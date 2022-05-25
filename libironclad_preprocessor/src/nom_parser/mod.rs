//! Quick scan through a source file, split it using preprocessor directives as a divider

use crate::nom_parser::pp_parse_types::{
  PpAstParserResult, PpParserResult, PpStringParserResult, PreprocessorParser,
  StrSliceParserResult, VecPpAstParserResult,
};
use crate::syntax_tree::pp_ast::PpAst;
use libironclad_erlang::syntax_tree::nom_parse::misc::{
  comma, newline_or_eof, par_close, par_open, parse_line_comment, period, ws, ws_before,
  ws_before_mut,
};
use libironclad_erlang::syntax_tree::nom_parse::parse_str::StringParser;
use nom::branch::alt;
use nom::character::complete::anychar;
use nom::combinator::{map, recognize, verify};
use nom::multi::{many0, many1, separated_list0};
use nom::sequence::{delimited, pair, preceded, tuple};
use nom::{
  bytes::complete::tag,
  character::complete::{alphanumeric1, char},
  error::context,
};

pub mod pp_parse_def_undef;
pub mod pp_parse_if;
pub mod pp_parse_types;

impl PreprocessorParser {
  // /// Parse a `Var1, Var2, ...` into a list
  // fn parse_comma_sep_varnames(input: &str) -> PpParserResult<Vec<String>> {
  //   separated_list0(comma, parse_varname)(input)
  // }

  /// Parse a `Macroident1, Macroident2, ...` into a list
  fn comma_sep_macro_idents(input: &str) -> PpParserResult<Vec<String>> {
    separated_list0(comma, Self::macro_ident)(input)
  }

  fn parenthesis_dot_newline(input: &str) -> PpParserResult<&str> {
    recognize(tuple((par_close, period, newline_or_eof)))(input)
  }

  /// Parse an identifier, starting with a letter and also can be containing numbers and underscoress
  fn macro_ident(input: &str) -> PpStringParserResult {
    map(
      recognize(pair(
        verify(anychar, |c: &char| c.is_alphabetic() || *c == '_'),
        many0(alt((alphanumeric1, tag("_")))),
      )),
      |result: &str| result.to_string(),
    )(input)
  }

  /// Parse a `-include(STRING)`
  fn include_directive(input: &str) -> PpAstParserResult {
    map(
      preceded(
        Self::match_dash_tag("include"),
        delimited(par_open, ws_before(StringParser::parse_string), par_close),
      ),
      PpAst::new_include,
    )(input)
  }

  /// Parse a `-include_lib(STRING)`
  fn include_lib_directive(input: &str) -> PpAstParserResult {
    map(
      preceded(
        Self::match_dash_tag("include_lib"),
        delimited(par_open, ws_before(StringParser::parse_string), par_close),
      ),
      PpAst::new_include_lib,
    )(input)
  }

  /// Returns a new parser which recognizes a `<spaces> "-" <spaces> <tag>` and returns it as
  /// a `&str` slice (*recognizes*, i.e. returns with all whitespace included)
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
      context("'-define' directive", Self::define_directive),
      context("'-undef' directive", Self::undef_directive),
      // temporary nodes used by parse_if_block
      context("'-endif' directive", Self::endif_temporary_directive),
      context("'-elif' directive", Self::elif_temporary_directive),
      context("'-else' directive", Self::else_temporary_directive),
      context("'-ifdef' directive", Self::ifdef_temporary_directive),
      context("'-ifndef' directive", Self::ifndef_temporary_directive),
      context("'-if' directive", Self::if_directive), // if must go after longer words ifdef and ifndef
      // Self::parse_error,
      // Self::parse_warning,
      context("'-include_lib' directive", Self::include_lib_directive),
      context("'-include' directive", Self::include_directive),
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
