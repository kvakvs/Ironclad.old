//! Trait for grouping together postprocessing functions in AST content structs
use std::rc::Rc;

use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::erl_module::ErlModule;

/// Implement this on AST node content structs
pub trait TPostProcess {
  /// Possibly replace some fields in self and return new AST, or return original AST. Env arg
  /// is providing the environment access (to find functions, other modules, etc).
  fn postprocess_ast(&self, env: &mut ErlModule, ast: Rc<ErlAst>) -> Option<Rc<ErlAst>>;
}
