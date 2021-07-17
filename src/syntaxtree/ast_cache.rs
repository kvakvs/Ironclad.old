use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use crate::project::source_file::SourceFile;
use std::rc::Rc;

/// Defines AST tree for a Erlang or Preprocessor-parsed module.
#[derive(Debug, Eq, PartialEq)]
pub struct AstTree<TNode> {
  pub source: Arc<SourceFile>,
  pub nodes: Rc<TNode>,
}

impl<TNode> AstTree<TNode> {
  pub fn new(source_file: Arc<SourceFile>, forms: Rc<TNode>) -> Self {
    Self {
      source: source_file,
      nodes: forms,
    }
  }
}

pub struct AstCache<TNode> {
  pub items: HashMap<PathBuf, Arc<AstTree<TNode>>>,
}

impl<TNode> AstCache<TNode> {
  pub fn new_empty() -> Self {
    Self { items: Default::default() }
  }
}
