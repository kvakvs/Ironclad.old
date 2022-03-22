//! Quick scan through a source file, split it using preprocessor directives as a divider

use std::sync::Arc;
use nom::{combinator, sequence, multi, character::complete::{char},
          bytes::complete::{tag}, branch, combinator::{cut}, error::{context}};
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
  /// Split input into AST nodes for preprocessor directives and any irrelevant text in between
  fn parse_fragments_collection(input: &str) -> VecAstParserResult {
    multi::many0(

    )(input)
  }

  /// Parses module contents, must begin with `-module()` attr followed by 0 or more module forms.
  pub fn parse_module(input: &str) -> AstParserResult {
    let (input, fragments) = Self::parse_fragments_collection(input)?;
    Ok((input, PpAst::new_file(fragments)))
  }
}
