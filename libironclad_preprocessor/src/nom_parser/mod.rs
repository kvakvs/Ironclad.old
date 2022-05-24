//! Quick scan through a source file, split it using preprocessor directives as a divider

use crate::nom_parser::pp_parse_types::{
  PpAstParserResult, PpParserResult, PpStringParserResult, PreprocessorParser, VecPpAstParserResult,
};
use crate::syntax_tree::pp_ast::PpAst;
use libironclad_erlang::syntax_tree::nom_parse::misc::{
  newline_or_eof, parse_line_comment, parse_varname, ws, ws_before, ws_before_mut,
};
use libironclad_erlang::syntax_tree::nom_parse::parse_str::StringParser;
use nom::{
  branch,
  bytes::complete::tag,
  character,
  character::complete::{alphanumeric1, char},
  combinator,
  error::context,
  multi, sequence,
};

pub mod pp_parse_if;
pub mod pp_parse_types;

impl PreprocessorParser {
  /// Parse a `Var1, Var2, ...` into a list
  fn parse_comma_sep_varnames(input: &str) -> PpParserResult<Vec<String>> {
    multi::separated_list0(ws_before(char(',')), parse_varname)(input)
  }

  fn terminator(input: &str) -> PpParserResult<&str> {
    combinator::recognize(sequence::tuple((
      ws_before(char(')')),
      ws_before(char('.')),
      newline_or_eof,
    )))(input)
  }

  /// Parse a `-define(NAME)` or `-define(NAME, VALUE)` or `-define(NAME(ARGS,...), VALUE)`
  pub fn parse_define(input: &str) -> PpAstParserResult {
    combinator::map(
      sequence::preceded(
        ws_before(tag("define")),
        sequence::tuple((
          ws_before(char('(')),
          ws_before(parse_varname),
          combinator::opt(sequence::delimited(
            ws_before(char('(')),
            Self::parse_comma_sep_varnames,
            ws_before(char(')')),
          )),
          combinator::opt(sequence::delimited(
            ws_before(char(',')),
            multi::many_till(nom::character::complete::anychar, Self::terminator),
            Self::terminator,
          )), // )delimited )opt
          ws_before(char(')')),
        )),
      ),
      |(_open, name, args, body, _close)| {
        PpAst::new_define(
          name,
          args,
          body.map(|(chars, _term)| chars.into_iter().collect::<String>()),
        )
      },
    )(input)
  }

  /// Parse an identifier, starting with lowercase and also can be containing numbers and underscoress
  fn parse_macro_ident(input: &str) -> PpStringParserResult {
    combinator::map(
      combinator::recognize(sequence::pair(
        combinator::verify(character::complete::anychar, |c: &char| c.is_alphabetic() || *c == '_'),
        multi::many1(branch::alt((alphanumeric1, tag("_")))),
      )),
      |result: &str| result.to_string(),
    )(input)
  }

  /// Parse a `-undef(IDENT)`
  fn parse_undef(input: &str) -> PpAstParserResult {
    combinator::map(
      sequence::preceded(
        ws_before(tag("undef")),
        sequence::tuple((
          ws_before(char('(')),
          ws_before(Self::parse_macro_ident),
          ws_before(char(')')),
        )),
      ),
      |(_open, ident, _close)| PpAst::new_undef(ident),
    )(input)
  }

  /// Parse a `-include(STRING)`
  fn parse_include(input: &str) -> PpAstParserResult {
    combinator::map(
      sequence::preceded(
        ws_before(tag("include")),
        sequence::tuple((
          ws_before(char('(')),
          ws_before(StringParser::parse_string),
          ws_before(char(')')),
        )),
      ),
      |(_open, s, _close)| PpAst::new_include(s),
    )(input)
  }

  /// Parse a `-include_lib(STRING)`
  fn parse_include_lib(input: &str) -> PpAstParserResult {
    combinator::map(
      sequence::preceded(
        ws_before(tag("include_lib")),
        sequence::tuple((
          ws_before(char('(')),
          ws_before(StringParser::parse_string),
          ws_before(char(')')),
        )),
      ),
      |(_open, s, _close)| PpAst::new_include_lib(s),
    )(input)
  }

  fn parse_preproc_directive(input: &str) -> PpAstParserResult {
    sequence::delimited(
      // Preceded by a dash -
      ws_before(char('-')),
      ws_before_mut(branch::alt((branch::alt((
        // -define is special, it needs closing ).\n to consume the content
        context("'-define' directive", Self::parse_define),
        context("'-undef' directive", Self::parse_undef),
        // temporary nodes used by parse_if_block
        context("'-elif' directive", Self::parse_elif_temporary),
        context("'-else' directive", Self::parse_else_temporary),
        context("'-ifdef' directive", Self::parse_ifdef_temporary),
        context("'-ifndef' directive", Self::parse_ifndef_temporary),
        context("'-if' directive", Self::parse_if_block), // if must go after longer words ifdef and ifndef
        // Self::parse_error,
        // Self::parse_warning,
        context("'-include_lib' directive", Self::parse_include_lib),
        context("'-include' directive", Self::parse_include),
      )),))),
      // Terminated by .\n
      sequence::pair(ws_before(char('.')), newline_or_eof),
    )(input)
  }

  /// Parse full lines till a line which looks like a preprocessor directive is found
  fn consume_one_line_of_text(input: &str) -> PpAstParserResult {
    combinator::map(
      combinator::verify(
        ws(nom::bytes::complete::take_till(|c| c == '\n' || c == '\r')),
        |text: &str| !text.is_empty(), //&& !text.starts_with('-'),
      ),
      PpAst::new_text,
    )(input)
  }

  /// Parses either a preprocessor directive or block, or consumes one line of text
  fn parse_fragment(input: &str) -> PpAstParserResult {
    branch::alt((
      Self::parse_preproc_directive,
      Self::consume_one_line_of_text,
      // A final comment in file is not visible to consume_text
      combinator::map(parse_line_comment, |_| PpAst::new_text("")),
    ))(input)
  }

  /// Split input into AST nodes for preprocessor directives and any irrelevant text in between
  pub fn parse_fragments_collection(input: &str) -> VecPpAstParserResult {
    // Followed by 1 or more directive or another text fragment
    multi::many1(Self::parse_fragment)(input)
  }

  /// Parses file contents into mix of preprocessor directives and text fragments.
  /// Comments are eliminated.
  pub fn parse_module(input: &str) -> PpAstParserResult {
    let (input, fragments) = Self::parse_fragments_collection(input)?;
    Ok((input, PpAst::new_file(fragments)))
  }
}
