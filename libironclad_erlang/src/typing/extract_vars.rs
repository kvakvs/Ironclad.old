//! Analyze AST and extract new variables from it

use crate::syntax_tree::erl_ast::{ErlAst, ErlAstType};
use crate::syntax_tree::node::erl_var::ErlVar;
use ::function_name::named;

/// Hosts code to extract new introduced variables from Core AST
pub struct ExtractVar {}

impl ExtractVar {
  /// For `CoreAst` return a vector of all new variables introduced from this AST
  #[named]
  pub fn extract_vars(ast: &ErlAst) -> Vec<ErlVar> {
    match &ast.content {
      ErlAstType::Var(v) => vec![v.clone()],
      ErlAstType::Lit { .. } => vec![],
      other => {
        unimplemented!("{}/{}: Don't know how to handle {:?}", file!(), function_name!(), other)
      }
    }
  }
}
