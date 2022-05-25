//! Checks for Preprocessor AST nodes

use crate::syntax_tree::pp_ast::PpAst;

impl PpAst {
  /// Check whether Preprocessor AST node is a text node
  pub fn is_text(&self) -> bool {
    matches!(self, PpAst::Text(_))
  }

  /// Compare PpAst node with a given text sample
  pub fn is_text_of(&self, t: &str) -> bool {
    matches!(self, PpAst::Text(text) if text == t)
  }

  /// Check whether Preprocessor AST node is a temporary `-else` node
  pub fn is_else(&self) -> bool {
    matches!(self, PpAst::_TemporaryElse)
  }

  /// Check whether Preprocessor AST node is a temporary `-elseif` node
  pub fn is_elseif(&self) -> bool {
    matches!(self, PpAst::_TemporaryElseIf(_))
  }

  /// Check whether Preprocessor AST node is a temporary `-endif` node
  pub fn is_endif(&self) -> bool {
    matches!(self, PpAst::_TemporaryEndif)
  }
}
