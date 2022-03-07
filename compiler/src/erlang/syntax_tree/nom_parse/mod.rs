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

/// Gathers multiple errors and contexts together
pub type ErlParserError<'a> = nom::error::VerboseError<&'a str>;

/// Groups functions for parsing Erlang syntax together
pub struct ErlParser {}

impl ErlParser {
  /// Parses 0 or more module forms (attrs and function defs)
  pub fn parse_module_forms_collection(input: &str) -> nom::IResult<&str, Vec<Arc<ErlAst>>, ErlParserError> {
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
  pub fn parse_module_form(input: &str) -> nom::IResult<&str, Arc<ErlAst>, ErlParserError> {
    branch::alt((
      Self::parse_export_attr,
      Self::parse_import_attr,
      Self::parse_generic_attr,
      Self::parse_fn_spec,
      Self::parse_fndef,
    ))(input)
  }

  /// Parses module contents, must begin with `-module()` attr followed by 0 or more module forms.
  pub fn parse_module(input: &str) -> nom::IResult<&str, Arc<ErlAst>, ErlParserError> {
    let (input, m_attr) = Self::parse_module_attr(input)?;
    println!("Parsed module attr: {}", m_attr);

    let (input, mut forms) = Self::parse_module_forms_collection(input)?;

    forms.insert(0, m_attr);
    Ok((input, ErlAst::ModuleForms(forms).into()))
  }
}