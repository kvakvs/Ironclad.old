//! Parsers for Erlang syntax based on Nom

use std::sync::Arc;

use crate::erl_syntax::erl_ast::ErlAst;
use crate::erl_syntax::erl_ast::ErlAstType::ModuleForms;
use crate::erl_syntax::parsers::parse_attr::ErlAttrParser;
use nom::branch::alt;
use nom::character::complete::multispace0;
use nom::combinator::{complete, map};
use nom::multi::many0;

pub mod misc;
pub mod parse_atom;
pub mod parse_attr;
pub mod parse_binary;
pub mod parse_case;
pub mod parse_expr;
pub mod parse_expr_op;
pub mod parse_fn;
pub mod parse_if_stmt;
pub mod parse_lit;
pub mod parse_str;
pub mod parse_try_catch;
pub mod parse_type;

/// Gathers multiple errors and contexts together
pub type ErlParserError<'a> = nom::error::VerboseError<&'a str>;

/// Generic return value from a Nom parser which takes &str and returns `Out`
pub type ParserResult<'a, Out> = nom::IResult<&'a str, Out, ErlParserError<'a>>;

/// Return value from a Nom parser which takes &str and returns `Arc<ErlAst>`
pub type AstParserResult<'a> = ParserResult<'a, Arc<ErlAst>>;

/// Return value from a Nom parser which takes &str and returns `Vec<Arc<ErlAst>>`
pub type VecAstParserResult<'a> = ParserResult<'a, Vec<Arc<ErlAst>>>;

/// Return value from a Nom parser which takes &str and returns `String`
pub type StringParserResult<'a> = ParserResult<'a, String>;

/// Return value from a Nom parser which takes &str and returns `&str`
pub type StrSliceParserResult<'a> = ParserResult<'a, &'a str>;

/// Return value from a Nom parser which takes &str and returns `()`
pub type VoidParserResult<'a> = ParserResult<'a, ()>;

/// Groups functions for parsing Erlang syntax together
pub struct ErlParser {}

impl ErlParser {
  /// Parses 0 or more module forms (attrs and function defs)
  pub fn parse_module_forms_collection(input: &str) -> VecAstParserResult {
    // Skip whitespace and check if no input remaining
    let (input, _skip_space) = multispace0(input)?;
    if input.is_empty() {
      return Ok((input, Vec::default()));
    }

    many0(complete(Self::parse_module_form))(input)
  }

  /// Parses an attribute or a function def
  pub fn parse_module_form(input: &str) -> AstParserResult {
    alt((ErlAttrParser::parse, Self::parse_fndef))(input)
  }

  /// Parses module contents, must begin with `-module()` attr followed by 0 or more module forms.
  pub fn parse_module(input: &str) -> AstParserResult {
    map(Self::parse_module_forms_collection, |forms| {
      ErlAst::construct_without_location(ModuleForms(forms))
    })(input)
  }
}
