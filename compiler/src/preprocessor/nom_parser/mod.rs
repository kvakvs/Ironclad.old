//! Quick scan through a source file, split it using preprocessor directives as a divider

use std::sync::Arc;
use nom::{combinator, sequence, multi, character::complete::{char},
          bytes::complete::{tag}, branch, combinator::{cut}, error::{context}};
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
  fn parse_define(input: &str) -> AstParserResult {
    combinator::map(
      sequence::preceded(
        tag("define"),
        sequence::tuple((
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
          )) // )delimited )opt
        )),
      ),
      |(name, args, body)| {
        PpAst::new_define(
          name,
          args,
          body.map(|(chars, _term)| chars.into_iter().collect::<String>()))
      },
    )(input)
  }

  fn parse_preproc_directive(input: &str) -> AstParserResult {
    context("preprocessor directive", cut(
      sequence::delimited(
        MiscParser::ws_before(char('-')),
        MiscParser::ws_before_mut(branch::alt((
          branch::alt((
            Self::parse_define, // -define is special, it needs closing ).\n to consume the content
            // Self::parse_undef,
            // Self::parse_error,
            // Self::parse_warning,
            // Self::parse_include_lib,
            // Self::parse_include,
          )),
          // branch::alt((
          //   Self::parse_if,
          //   Self::parse_ifdef,
          //   Self::parse_ifndef,
          //   Self::parse_elif,
          //   Self::parse_else,
          //   Self::parse_endif,
          // )),
        ))),
        // Terminated by .\n
        sequence::pair(
          MiscParser::ws_before(char('.')),
          MiscParser::newline_or_eof),
      )
    ))(input)
  }

  /// Parse full lines till a line which looks like a preprocessor directive is found
  fn consume_lines(input: &str) -> AstParserResult {
    combinator::map(
      combinator::verify(
        MiscParser::ws_before(nom::bytes::complete::take_till(|c| c == '\n' || c == '\r')),
        |text: &str| !text.is_empty() && !text.starts_with('-'),
      ),
      |text| PpAst::new_text(text.into()),
    )(input)
  }

  /// Split input into AST nodes for preprocessor directives and any irrelevant text in between
  fn parse_fragments_collection(input: &str) -> VecAstParserResult {
    combinator::map(
      sequence::pair(
        Self::consume_lines,
        multi::many1(
          sequence::pair(
            Self::parse_preproc_directive,
            Self::consume_lines,
          )
        ),
      ),
      |(lines, pairs)| {
        let mut result = vec![lines];
        for (directive, text) in pairs {
          result.push(directive);
          result.push(text);
        }
        result
      },
    )(input)
  }

  /// Parses module contents, must begin with `-module()` attr followed by 0 or more module forms.
  pub fn parse_module(input: &str) -> AstParserResult {
    let (input, fragments) = Self::parse_fragments_collection(input)?;
    Ok((input, PpAst::new_file(fragments)))
  }
}
