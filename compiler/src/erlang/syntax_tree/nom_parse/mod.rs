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

/// Parses an attribute or a function def
pub fn parse_module_form(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  branch::alt((
    parse_attr::parse_generic_attr,
    parse_fn::parse_fndef,
  ))(input)
}

/// Parses module contents, must begin with `-module()` attr followed by 0 or more module forms.
pub fn parse_module(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  combinator::map(
    sequence::pair(
      parse_attr::parse_module_attr,
      multi::many0(parse_module_form),
    ),
    |(m_attr, mut forms)| {
      forms.insert(0, m_attr);
      ErlAst::ModuleForms(forms).into()
    },
  )(input)
}
