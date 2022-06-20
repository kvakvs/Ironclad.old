//! Defines an Erlang module ready to be compiled
use crate::erl_syntax::erl_ast::AstNode;
use mod_impl::ErlModule;
use std::sync::{Arc, RwLock};

use crate::erl_syntax::parsers::parser_scope::ParserScope;
use crate::project::module::scope::root_scope::RootScope;
use scope::scope_impl::Scope;

pub mod mod_impl;
pub mod mod_parse;
pub mod mod_preprocess;
pub mod scope;

/// Read accessor
pub fn erl_module_ast(m: &ErlModule) -> AstNode {
  if let Ok(read_m) = m.read() {
    read_m.ast.clone()
  } else {
    panic!("Can't lock module to access AST")
  }
}

/// Read accessor
pub fn erl_module_parser_scope(m: &ErlModule) -> ParserScope {
  if let Ok(read_m) = m.read() {
    read_m.parser_scope.clone()
  } else {
    panic!("Can't lock module to access parser scope")
  }
}

/// Read accessor
pub fn erl_module_root_scope(m: &ErlModule) -> RootScope {
  if let Ok(read_m) = m.read() {
    read_m.root_scope.clone()
  } else {
    panic!("Can't lock module to access module scope")
  }
}
