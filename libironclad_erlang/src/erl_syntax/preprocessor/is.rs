//! Checks for Preprocessor AST node types

use crate::erl_syntax::erl_ast::{ErlAst, ErlAstType};
use crate::erl_syntax::preprocessor::ast::PreprocessorNodeType;

impl ErlAst {
  /// Check whether Preprocessor AST node is a temporary `-else` node
  pub fn is_else(&self) -> bool {
    matches!(&self.content, ErlAstType::Preprocessor(PreprocessorNodeType::_TemporaryElse))
  }

  /// Check whether Preprocessor AST node is a temporary `-elseif` node
  pub fn is_elseif(&self) -> bool {
    matches!(
      &self.content,
      ErlAstType::Preprocessor(PreprocessorNodeType::_TemporaryElseIf(_))
    )
  }

  /// Check whether Preprocessor AST node is a temporary `-endif` node
  pub fn is_endif(&self) -> bool {
    matches!(&self.content, ErlAstType::Preprocessor(PreprocessorNodeType::_TemporaryEndif))
  }
}
