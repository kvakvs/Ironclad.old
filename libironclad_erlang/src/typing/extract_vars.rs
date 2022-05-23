//! Analyze AST and extract new variables from it

use crate::syntax_tree::erl_ast::ErlAst;
use crate::syntax_tree::node::erl_var::ErlVar;

/// Hosts code to extract new introduced variables from Core AST
pub struct ExtractVar {}

impl ExtractVar {
  /// For `CoreAst` return a vector of all new variables introduced from this AST
  pub fn extract_vars(ast: &ErlAst) -> Vec<ErlVar> {
    match ast {
      ErlAst::Var(v) => vec![v.clone()],
      ErlAst::Lit { .. } => vec![],
      other => unimplemented!("ExtractVar: Don't know how to handle {}", other),
    }
  }
}
