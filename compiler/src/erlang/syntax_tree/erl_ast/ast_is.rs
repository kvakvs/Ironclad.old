//! AST node-type checks

use crate::erlang::syntax_tree::erl_ast::ErlAst;

impl ErlAst {
  /// Checks whether a CoreAST node is a function definition
  pub fn is_fndef(&self) -> bool { matches!(self, ErlAst::FnDef(_)) }
}
