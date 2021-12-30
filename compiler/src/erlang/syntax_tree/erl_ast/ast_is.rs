//! AST node-type checks

use crate::erlang::syntax_tree::erl_ast::ErlAst;

impl ErlAst {
  /// Checks whether an ErlAst node is a function definition
  pub fn is_fn_def(&self) -> bool { matches!(self, ErlAst::FnDef(_)) }

  /// Checks whether an ErlAst node is a function spec
  pub fn is_fn_spec(&self) -> bool { matches!(self, ErlAst::FnSpec{..}) }
}
