//! Parsers for Erlang syntax based on Nom

use std::sync::Arc;

use nom::{combinator, multi, branch};

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

    combinator::complete(
      multi::many0(Self::parse_module_form),
    )(input)
  }

  /// Parses an attribute or a function def
  pub fn parse_module_form(input: &str) -> nom::IResult<&str, Arc<ErlAst>, ErlParserError> {
    branch::alt((
      Self::parse_generic_attr,
      Self::parse_fndef,
    ))(input)
  }

  /// Parses module contents, must begin with `-module()` attr followed by 0 or more module forms.
  pub fn parse_module(input: &str) -> nom::IResult<&str, Arc<ErlAst>, ErlParserError> {
    let (input, m_attr) = Self::parse_module_attr(input)?;
    println!("Parsed module attr: {}; input=«{}»", m_attr, input);

    let (input, mut forms) = Self::parse_module_forms_collection(input)?;

    forms.insert(0, m_attr);
    Ok((input, ErlAst::ModuleForms(forms).into()))
  }
}