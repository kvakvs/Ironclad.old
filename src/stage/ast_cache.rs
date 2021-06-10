use std::collections::HashMap;
use std::path::PathBuf;
use crate::erl_parse::ast::ErlAstNode;

// pub enum ASTNode {
//     Attribute,
//     Function,
// }

pub struct ModuleAST {
  pub forms: Vec<ErlAstNode>,
}

impl ModuleAST {
  pub fn new(forms: Vec<ErlAstNode>) -> Self {
    Self { forms }
  }
}

pub struct AstCache {
  pub syntax_trees: HashMap<PathBuf, ModuleAST>,
}

impl AstCache {
  pub fn new() -> Self {
    Self { syntax_trees: Default::default() }
  }
}
