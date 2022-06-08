#![allow(dead_code)]

use libironclad::project::module::ErlModule;
use libironclad_erlang::erl_syntax::erl_ast::ast_iter::AstParentNodeT;
use libironclad_erlang::erl_syntax::erl_ast::AstNode;
use std::path::PathBuf;

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
pub fn parse_a_module(function_name: &str, input: &str) -> Vec<AstNode> {
  let input = format!("-module({}).\n{}", function_name, input);
  // println!("IN=«{}»", input);
  let filename = PathBuf::from(function_name);
  let module = ErlModule::from_module_source(&filename, &input).unwrap();

  let (mod_name, _nodes) = module.ast.as_module();
  assert_eq!(mod_name, function_name);

  // println!("Parsed=«{}»", module.ast);
  module.ast.children().unwrap_or_default()
}
