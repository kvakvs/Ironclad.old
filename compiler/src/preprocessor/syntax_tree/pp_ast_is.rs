//! Checks for Preprocessor AST nodes

use crate::preprocessor::syntax_tree::pp_ast::PpAst;

impl PpAst {
  /// Check whether Preprocessor AST node is a temporary `-else` node
  pub fn is_else(&self) -> bool {
    matches!(self, PpAst::_TemporaryElse)
  }

  /// Check whether Preprocessor AST node is a temporary `-elseif` node
  pub fn is_elseif(&self) -> bool {
    matches!(self, PpAst::_TemporaryElseIf)
  }
}
