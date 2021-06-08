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

pub struct ASTCache {
    pub syntax_trees: HashMap<PathBuf, ModuleAST>,
}

impl ASTCache {
    pub fn new() -> Self {
        Self { syntax_trees: Default::default() }
    }
}
