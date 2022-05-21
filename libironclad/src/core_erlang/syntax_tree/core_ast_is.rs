//! Checking CoreAst variants
#![cfg(coreast)]

use crate::core_erlang::syntax_tree::core_ast::CoreAst;

impl CoreAst {
  /// Checks whether a CoreAST node is a function definition
  pub fn is_fndef(&self) -> bool { matches!(self, CoreAst::FnDef(_)) }
}
