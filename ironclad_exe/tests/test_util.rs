#![allow(dead_code)]

use ::function_name::named;
use libironclad_erlang::erl_syntax::erl_ast::ast_iter::IterableAstNodeT;
use libironclad_erlang::erl_syntax::erl_ast::AstNode;
use libironclad_erlang::erl_syntax::token_stream::token::Token;
use libironclad_erlang::erl_syntax::token_stream::tokenizer::tokenize_source;
use libironclad_erlang::project::module::module_impl::{ErlModule, ErlModuleImpl};
use libironclad_erlang::project::project_impl::ErlProjectImpl;
use libironclad_erlang::typing::erl_type::ErlType;
use libironclad_util::source_file::SourceFileImpl;
use std::path::PathBuf;
use std::sync::Arc;

pub fn fail_unexpected<T>(val: &T)
where
  T: std::fmt::Debug,
{
  panic!("Unexpected value: {:?}", val)
}

pub fn start(n: &str, descr: &str) {
  println!("▼╍╍╍╍╍╍ {} ╍╍╍ ({}) ╍╍╍╍╍╍", n, descr);
}

/// Try parse a define macro where value contains another macro
/// Returns `ErlModule`
#[named]
pub fn parse_module(function_name: &str, input: &str) -> ErlModule {
  let input = format!("-module({}).\n{}", function_name, input);
  let filename = PathBuf::from(function_name);

  println!("{}: Input=«{}»", function_name!(), input);

  let source_file = SourceFileImpl::new(&filename, input);
  let project = ErlProjectImpl::default().into();
  let module = ErlModuleImpl::from_module_source(&project, &source_file, None).unwrap();

  let ast = module.ast.borrow().clone();
  println!("{}: Out=«{}»", function_name!(), ast);

  // Unwrapping the ModuleRoot node
  // let nodes = ast.as_module();
  // assert_eq!(mod_name, function_name);

  module
}

pub fn tokenize(input: &str) -> Vec<Token> {
  let project = Arc::new(ErlProjectImpl::default());
  let src_file = SourceFileImpl::new(&PathBuf::from("test"), input.to_string());
  ErlModuleImpl::tokenize_helper(&project, src_file.clone(), tokenize_source).unwrap()
}

/// Try parse a define macro where value contains another macro
/// Returns `ErlModule.ast.children()`
pub fn parse_module_unwrap(function_name: &str, input: &str) -> Vec<AstNode> {
  let module = parse_module(function_name, input);
  let ast = module.ast.borrow().clone();
  ast.children().unwrap_or_default()
}

pub fn parse_expr(function_name: &str, input: &str) -> AstNode {
  let project = ErlProjectImpl::default().into();
  let source_file = SourceFileImpl::new(&PathBuf::from(function_name), input.to_string());
  let module = ErlModuleImpl::from_expr_source(&project, &source_file, None).unwrap();
  let ast_b = module.ast.borrow();
  ast_b.clone()
}

pub fn parse_type(function_name: &str, input: &str) -> ErlType {
  let project = ErlProjectImpl::default().into();
  let source_file = SourceFileImpl::new(&PathBuf::from(function_name), input.to_string());
  let module = ErlModuleImpl::from_type_source(&project, &source_file, None).unwrap();
  let ast_b = module.ast.borrow();
  ast_b.as_type()
}
