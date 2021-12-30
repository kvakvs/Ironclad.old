//! Parsers for Erlang syntax based on Nom

use std::sync::Arc;

use nom::{combinator, sequence, multi, branch};

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
  /// Parses an attribute or a function def
  pub fn parse_module_form(input: &str) -> nom::IResult<&str, Arc<ErlAst>, ErlParserError> {
    branch::alt((
      Self::parse_generic_attr,
      Self::parse_fndef,
    ))(input)
  }

  /// Parses module contents, must begin with `-module()` attr followed by 0 or more module forms.
  pub fn parse_module(input: &str) -> nom::IResult<&str, Arc<ErlAst>, ErlParserError> {
    combinator::map(
      sequence::pair(
        Self::parse_module_attr,
        multi::many0(Self::parse_module_form),
      ),
      |(m_attr, mut forms)| {
        forms.insert(0, m_attr);
        ErlAst::ModuleForms(forms).into()
      },
    )(input)
  }
}