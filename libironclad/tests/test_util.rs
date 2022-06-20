#![allow(dead_code)]

use libironclad_erlang::erl_syntax::erl_ast::ast_iter::IterableAstNodeT;
use libironclad_erlang::erl_syntax::erl_ast::AstNode;
use libironclad_erlang::project::module::erl_module_ast;
use libironclad_erlang::project::module::mod_impl::{ErlModule, ErlModuleImpl};
use libironclad_erlang::project::project_impl::ErlProjectImpl;
use libironclad_erlang::source_file::SourceFileImpl;
use libironclad_erlang::typing::erl_type::ErlType;
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
pub fn parse_module(function_name: &str, input: &str) -> ErlModule {
  let input = format!("-module({}).\n{}", function_name, input);
  let filename = PathBuf::from(function_name);

  println!("Input=«{}»", input);

  let source_file = SourceFileImpl::new(&filename, input);
  let project = ErlProjectImpl::default().into();
  let module = ErlModuleImpl::from_module_source(&project, &source_file, None).unwrap();

  if let Ok(r_module) = module.read() {
    println!("Out=«{}»", r_module.ast);
    let (mod_name, _nodes) = r_module.ast.as_module();
    assert_eq!(mod_name, function_name);
  } else {
    panic!("Can't lock module for read")
  }

  module
}

/// Try parse a define macro where value contains another macro
/// Returns `ErlModule.ast.children()`
pub fn parse_module_unwrap(function_name: &str, input: &str) -> Vec<AstNode> {
  let module = parse_module(function_name, input);

  if let Ok(r_module) = module.clone().read() {
    r_module.ast.children().unwrap_or_default()
  } else {
    panic!("Can't lock module for read")
  }
}

pub fn parse_expr(function_name: &str, input: &str) -> AstNode {
  let project = ErlProjectImpl::default().into();
  let source_file = SourceFileImpl::new(&PathBuf::from(function_name), input.to_string());
  let module = ErlModuleImpl::from_expr_source(&project, &source_file, None).unwrap();
  erl_module_ast(&module)
}

pub fn parse_type(function_name: &str, input: &str) -> Arc<ErlType> {
  let project = ErlProjectImpl::default().into();
  let source_file = SourceFileImpl::new(&PathBuf::from(function_name), input.to_string());
  let module = ErlModuleImpl::from_type_source(&project, &source_file, None).unwrap();
  erl_module_ast(&module).as_type()
}
