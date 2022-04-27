//! Quick scan through a source file, split it using preprocessor directives as a divider

use std::sync::Arc;
use nom::{combinator, sequence, multi, character::complete::{char},
          bytes::complete::{tag}, branch, error::{context}};
use crate::erlang::syntax_tree::nom_parse::ErlParser;
use crate::erlang::syntax_tree::nom_parse::misc::MiscParser;
use crate::preprocessor::syntax_tree::pp_ast::PpAst;

/// Gathers multiple errors and contexts together
pub type PpParserError<'a> = nom::error::VerboseError<&'a str>;

/// Generic return value from a Nom parser which takes &str and returns `Out`
pub type ParserResult<'a, Out> = nom::IResult<&'a str, Out, PpParserError<'a>>;

/// Return value from a Nom parser which takes &str and returns `Arc<PpAst>`
pub type AstParserResult<'a> = ParserResult<'a, Arc<PpAst>>;

/// Return value from a Nom parser which takes &str and returns `Vec<Arc<PpAst>>`
pub type VecAstParserResult<'a> = ParserResult<'a, Vec<Arc<PpAst>>>;

/// Return value from a Nom parser which takes &str and returns `String`
pub type StringParserResult<'a> = ParserResult<'a, String>;

/// Return value from a Nom parser which takes &str and returns `&str`
pub type StrSliceParserResult<'a> = ParserResult<'a, &'a str>;

/// Return value from a Nom parser which takes &str and returns `()`
pub type VoidParserResult<'a> = ParserResult<'a, ()>;

/// Groups code for parsing preprocessor directives
pub struct PreprocessorParser {}

impl PreprocessorParser {
  /// Parse a `Var1, Var2, ...` into a list
  fn parse_comma_sep_varnames(input: &str) -> ParserResult<Vec<String>> {
    multi::separated_list0(
      MiscParser::ws_before(char(',')),
      MiscParser::parse_varname,
    )(input)
  }

  fn terminator(input: &str) -> ParserResult<&str> {
    combinator::recognize(
      sequence::tuple((
        MiscParser::ws_before(char(')')),
        MiscParser::ws_before(char('.')),
        MiscParser::newline_or_eof,
      ))
    )(input)
  }

  /// Parse a `-define(NAME)` or `-define(NAME, VALUE)` or `-define(NAME(ARGS,...), VALUE)`
  pub fn parse_define(input: &str) -> AstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(tag("define")),
        sequence::tuple((
          MiscParser::ws_before(char('(')),
          MiscParser::ws_before(MiscParser::parse_varname),
          combinator::opt(sequence::delimited(
            MiscParser::ws_before(char('(')),
            Self::parse_comma_sep_varnames,
            MiscParser::ws_before(char(')')),
          )),
          combinator::opt(sequence::delimited(
            MiscParser::ws_before(char(',')),
            multi::many_till(
              nom::character::complete::anychar,
              Self::terminator),
            Self::terminator,
          )), // )delimited )opt
          MiscParser::ws_before(char(')')),
        )),
      ),
      |(_open, name, args, body, _close)| {
        PpAst::new_define(
          name,
          args,
          body.map(|(chars, _term)| chars.into_iter().collect::<String>()))
      },
    )(input)
  }

  /// Parse a `-if(EXPR)`
  fn parse_if(input: &str) -> AstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(tag("if")),
        sequence::tuple((
          MiscParser::ws_before(char('(')),
          MiscParser::ws_before(ErlParser::parse_expr),
          MiscParser::ws_before(char(')')),
        )),
      ),
      |(_open, expr, _close)| PpAst::new_if(expr),
    )(input)
  }

  /// Parse a `-elif(EXPR)`
  fn parse_elif(input: &str) -> AstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(tag("elif")),
        sequence::tuple((
          MiscParser::ws_before(char('(')),
          MiscParser::ws_before(ErlParser::parse_expr),
          MiscParser::ws_before(char(')')),
        )),
      ),
      |(_open, expr, _close)| PpAst::new_if(expr),
    )(input)
  }

  /// Parse a `-else.`
  fn parse_else(input: &str) -> AstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(tag("else")),
        combinator::opt(
          sequence::pair(
            MiscParser::ws_before(char('(')),
            MiscParser::ws_before(char(')')),
          )
        ),
      ),
      |_maybe_parens| PpAst::Else.into(),
    )(input)
  }

  /// Parse a `-endif.`
  fn parse_endif(input: &str) -> AstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(tag("endif")),
        combinator::opt(
          sequence::pair(
            MiscParser::ws_before(char('(')),
            MiscParser::ws_before(char(')')),
          )
        ),
      ),
      |_maybe_parens| PpAst::Endif.into(),
    )(input)
  }

  fn parse_preproc_directive(input: &str) -> AstParserResult {
    sequence::delimited(
      MiscParser::ws_before(char('-')),
      MiscParser::ws_before_mut(branch::alt((
        branch::alt((
          // -define is special, it needs closing ).\n to consume the content
          context("'-define' directive", Self::parse_define),
          // Self::parse_undef,
          // Self::parse_ifdef,
          // Self::parse_ifndef,
          context("'-if' directive", Self::parse_if), // must go after longer words ifdef and ifndef
          context("'-elif' directive", Self::parse_elif),
          context("'-else' directive", Self::parse_else),
          context("'-endif' directive", Self::parse_endif),
          // Self::parse_error,
          // Self::parse_warning,
          // Self::parse_include_lib,
          // Self::parse_include,
        )),
        // branch::alt((
        // )),
      ))),
      // Terminated by .\n
      sequence::pair(
        MiscParser::ws_before(char('.')),
        MiscParser::newline_or_eof),
    )(input)
  }

  /// Parse full lines till a line which looks like a preprocessor directive is found
  fn consume_text(input: &str) -> AstParserResult {
    combinator::map(
      combinator::verify(
        MiscParser::ws_before(nom::bytes::complete::take_till(|c| c == '\n' || c == '\r')),
        |text: &str| !text.is_empty() && !text.starts_with('-'),
      ),
      |text| PpAst::new_text(text.into()),
    )(input)
  }

  /// Split input into AST nodes for preprocessor directives and any irrelevant text in between
  pub fn parse_fragments_collection(input: &str) -> VecAstParserResult {
    // Followed by 1 or more directive or another text fragment
    multi::many1(
      branch::alt((
        Self::parse_preproc_directive,
        Self::consume_text,
      ))
    )(input)
  }

  /// Parses module contents, must begin with `-module()` attr followed by 0 or more module forms.
  pub fn parse_module(input: &str) -> AstParserResult {
    let (input, fragments) = Self::parse_fragments_collection(input)?;
    Ok((input, PpAst::new_file(fragments)))
  }
}
