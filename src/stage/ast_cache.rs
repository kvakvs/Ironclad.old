use std::collections::HashMap;
use std::path::PathBuf;
use crate::erl_parse::ast::ASTNode;

// pub enum ASTNode {
//     Attribute,
//     Function,
// }

pub struct ModuleAST {
  pub forms: Vec<ASTNode>,
}

impl ModuleAST {
  pub fn new(forms: Vec<ASTNode>) -> Self {
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
