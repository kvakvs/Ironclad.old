//! Parsers for Erlang syntax based on Nom

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::ws_mut;
use crate::erl_syntax::parsers::parse_attr::{module_start_attr, parse_module_attr};
use crate::erl_syntax::parsers::parse_fn::parse_fndef;
use crate::erl_syntax::preprocessor::parsers::preprocessor::parse_preproc_directive;
use defs::ParserInput;
use defs::VecAstParserResult;
use nom::branch::alt;
use nom::combinator::{complete, map};
use nom::error::context;
use nom::multi::many0;
use nom::sequence::pair;

pub mod defs;
pub mod misc;
pub mod parse_attr;
pub mod parse_binary;
pub mod parse_case;
pub mod parse_expr;
pub mod parse_expr_op;
pub mod parse_fn;
pub mod parse_if_stmt;
pub mod parse_lit;
pub mod parse_record;
pub mod parse_strings;
pub mod parse_try_catch;
pub mod parse_type;
pub mod parser_input;
pub mod parser_input_slice;
pub mod parser_scope;

/// Parses an attribute or a function def
pub(crate) fn parse_one_module_form(input: ParserInput) -> ParserResult<AstNode> {
  alt((
    parse_preproc_directive,
    parse_module_attr,
    context("function definition", parse_fndef),
  ))(input)
}

/// Parses 0 or more module forms (attrs and function defs)
pub fn parse_module_forms(input: ParserInput) -> VecAstParserResult {
  ws_mut(many0(complete(parse_one_module_form)))(input)
}

/// Parses module contents, must begin with `-module()` attr followed by 0 or more module forms.
pub fn parse_module(input: ParserInput) -> ParserResult<AstNode> {
  map(
    pair(module_start_attr, parse_module_forms),
    |(mod_name, forms): (String, Vec<AstNode>)| {
      AstNodeImpl::new_module_forms(input.loc(), mod_name, forms)
    },
  )(input.clone())
}
