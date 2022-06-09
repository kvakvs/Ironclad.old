#![allow(dead_code)]

use libironclad::project::module::ErlModule;
use libironclad_erlang::erl_syntax::erl_ast::ast_iter::IterableAstNodeT;
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
/// Returns `ErlModule`
pub fn parse_a_module0(function_name: &str, input: &str) -> ErlModule {
  let input = format!("-module({}).\n{}", function_name, input);
  let filename = PathBuf::from(function_name);

  println!("Input=«{}»", input);
  let module = ErlModule::from_module_source(&filename, &input).unwrap();
  println!("Out=«{}»", module.ast);

  let (mod_name, _nodes) = module.ast.as_module();
  assert_eq!(mod_name, function_name);
  module
}

/// Try parse a define macro where value contains another macro
/// Returns `ErlModule.ast.children()`
pub fn parse_module_unwrap(function_name: &str, input: &str) -> Vec<AstNode> {
  let module = parse_a_module0(function_name, input);
  module.ast.children().unwrap_or_default()
}
