//! Analyze `CoreAST` and extract new variables from it

use std::sync::Arc;
use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::core_erlang::syntax_tree::node::var::Var;

/// Hosts code to extract new introduced variables from Core AST
pub struct ExtractVar {}

impl ExtractVar {
  /// For `CoreAst` return a vector of all new variables introduced from this AST
  pub fn extract_vars(ast: &CoreAst) -> Vec<Arc<Var>> {
    match ast {
      CoreAst::Var(v) => vec![v.clone()],
      CoreAst::Lit { .. } => vec![],
      other => unimplemented!("ExtractVar: Don't know how to handle {}", other),
    }
  }
}