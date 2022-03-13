//! Parsers for Erlang syntax based on Nom

use std::sync::Arc;

use nom::{combinator, sequence, multi, branch, character};

use crate::erlang::syntax_tree::erl_ast::ErlAst;

pub mod parse_attr;
pub mod misc;
pub mod parse_atom;
pub mod parse_fn;
pub mod parse_expr;
pub mod parse_expr_op;
pub mod parse_lit;
pub mod parse_str;
pub mod parse_type;
pub mod parse_case;

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
    let (input, _skip_space) = nom::character::complete::multispace0(input)?;
    if input.is_empty() {
      return Ok((input, Vec::default()));
    }

    multi::many0(combinator::complete(Self::parse_module_form))(input)
  }

  /// Recognizes newline or end of input
  fn newline_or_eof<'a, ErrType: nom::error::ParseError<&'a str>>(
    input: &'a str
  ) -> nom::IResult<&str, &str, ErrType> {
    combinator::recognize(
      branch::alt((
        nom::bytes::complete::tag("\r\n"),
        nom::bytes::complete::tag("\r"),
        nom::bytes::complete::tag("\n"),
        combinator::eof,
      ))
    )(input)
  }

  /// Recognizes `% text <newline>` consuming text
  fn parse_line_comment<'a, ErrType: nom::error::ParseError<&'a str>>(
    input: &'a str
  ) -> nom::IResult<&str, &str, ErrType> {
    combinator::recognize(
      sequence::pair(
        multi::many1(
          character::complete::char('%'),
        ),
        multi::many_till(
          character::complete::anychar,
          Self::newline_or_eof,
        ),
      )
    )(input)
  }

  /// Parses an attribute or a function def
  pub fn parse_module_form(input: &str) -> AstParserResult {
    branch::alt((
      Self::parse_export_attr,
      Self::parse_import_attr,
      Self::parse_generic_attr,
      Self::parse_fn_spec,
      Self::parse_fndef,
    ))(input)
  }

  /// Parses module contents, must begin with `-module()` attr followed by 0 or more module forms.
  pub fn parse_module(input: &str) -> AstParserResult {
    let (input, m_attr) = Self::parse_module_attr(input)?;
    println!("Parsed module attr: {}", m_attr);

    let (input, mut forms) = Self::parse_module_forms_collection(input)?;

    forms.insert(0, m_attr);
    Ok((input, ErlAst::ModuleForms(forms).into()))
  }
}