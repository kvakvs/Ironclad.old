//! Checks for Preprocessor AST node types

use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::preprocessor::ast::PreprocessorNodeType;

impl AstNodeImpl {
  /// Check whether Preprocessor AST node is a temporary `-else` node
  pub(crate) fn is_else(&self) -> bool {
    matches!(&self.content, AstNodeType::Preprocessor(PreprocessorNodeType::_TemporaryElse))
  }

  /// Check whether Preprocessor AST node is a temporary `-elseif` node
  pub(crate) fn is_elseif(&self) -> bool {
    matches!(
      &self.content,
      AstNodeType::Preprocessor(PreprocessorNodeType::_TemporaryElseIf(_))
    )
  }

  /// Check whether Preprocessor AST node is a temporary `-endif` node
  pub(crate) fn is_endif(&self) -> bool {
    matches!(&self.content, AstNodeType::Preprocessor(PreprocessorNodeType::_TemporaryEndif))
  }
}
