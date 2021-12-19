//! Type unwrappers for CoreAST subtypes

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::core_erlang::syntax_tree::node::core_fn_def::FnDef;

impl CoreAst {
  /// Return CoreAst::FnDef node's content, or panic
  pub fn as_fndef(&self) -> &FnDef {
    match self {
      CoreAst::FnDef(fndef) => &fndef,
      _other => panic!("Expected CoreAst::FnDef element, got {}", self)
    }
  }
}