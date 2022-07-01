//! Defines an Erlang module ready to be compiled

pub mod module_impl;
pub mod module_parse;
pub mod module_verify;
pub mod preprocess;
pub mod scope;

// /// Read accessor
// pub fn erl_module_ast(m: &ErlModule) -> AstNode {
//   m.ast.borrow().clone()
//   // if let Ok(read_m) = m.read() {
//   //   read_m.ast.clone()
//   // } else {
//   //   panic!("Can't lock module to access AST")
//   // }
// }

// /// Read accessor
// pub fn erl_module_parser_scope(m: &ErlModule) -> ParserScope {
//   m.parser_scope.clone()
//   // if let Ok(read_m) = m.read() {
//   //   read_m.parser_scope.clone()
//   // } else {
//   //   panic!("Can't lock module to access parser scope")
//   // }
// }

// /// Read accessor
// pub fn erl_module_root_scope(m: &ErlModule) -> RootScope {
//   m.root_scope.clone()
//   // if let Ok(read_m) = m.read() {
//   //   read_m.root_scope.clone()
//   // } else {
//   //   panic!("Can't lock module to access module scope")
//   // }
// }
