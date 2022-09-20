//! Parsers for Erlang syntax based on Nom

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::parse_fn::parse_fndef;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use nom::combinator::{complete, map};
use nom::error::context;
use nom::multi::many0;

pub mod defs;
pub mod error_report;
pub mod lang_construct;
pub mod misc;
pub mod misc_tok;
pub mod parse_binary;
pub mod parse_case;
pub mod parse_expr;
pub mod parse_expr_op;
pub mod parse_fn;
pub mod parse_if;
pub mod parse_lit;
pub mod parse_lit_numbers;
pub mod parse_try_catch;
pub mod parse_type;
pub mod parser_error;
pub mod parser_input;
pub mod parser_input_slice;
pub mod preproc_defines;
pub mod token_stream;

/// Parses an attribute or a function def
pub(crate) fn parse_one_module_form(input: ParserInput) -> ParserResult<AstNode> {
  // Do not parse attributes and preprocessor here, it is done earlier in stage_preprocess stage after tokenizer
  context("function definition", parse_fndef)(input)
}

/// Parses 0 or more module forms (attrs and function defs)
pub fn parse_module_forms(input: ParserInput) -> ParserResult<Vec<AstNode>> {
  complete(many0(parse_one_module_form))(input)
}

/// Parses module contents, must begin with `-module()` attr followed by 0 or more module forms.
pub fn parse_module(input: ParserInput) -> ParserResult<AstNode> {
  map(parse_module_forms, AstNodeImpl::new_module_forms)(input.clone())
}
