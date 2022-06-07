//! Parsers for Erlang syntax based on Nom

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::node_impl::ErlAstType::ModuleForms;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::ws_mut;
use crate::erl_syntax::parsers::parse_attr::parse_module_attr;
use crate::erl_syntax::preprocessor::parsers::preprocessor_parser::PreprocessorParser;
use defs::ParserInput;
use defs::VecAstParserResult;
use nom::branch::alt;
use nom::combinator::{complete, map};
use nom::multi::many0;

pub mod defs;
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
pub mod parse_record;
pub mod parse_str;
pub mod parse_try_catch;
pub mod parse_type;
pub mod parser_input;
pub mod parser_input_slice;

/// Groups functions for parsing Erlang syntax together
pub struct ErlParser {}

impl ErlParser {
  /// Parses an attribute or a function def
  pub fn module_form(input: ParserInput) -> ParserResult<AstNode> {
    alt((
      PreprocessorParser::parse_preproc_directive,
      parse_module_attr,
      Self::parse_fndef,
    ))(input)
  }

  /// Parses 0 or more module forms (attrs and function defs)
  pub fn module_forms_collection(input: ParserInput) -> VecAstParserResult {
    ws_mut(many0(complete(Self::module_form)))(input)
  }

  /// Parses module contents, must begin with `-module()` attr followed by 0 or more module forms.
  pub fn parse_module(input: ParserInput) -> ParserResult<AstNode> {
    map(Self::module_forms_collection, |forms| {
      AstNodeImpl::construct_without_location(ModuleForms(forms))
    })(input)
  }
}
