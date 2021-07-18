//! Defines a cache for parsed AST trees
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use crate::project::source_file::SourceFile;
use std::rc::Rc;

/// Defines AST tree for a Erlang or Preprocessor-parsed module.
#[derive(Debug, Eq, PartialEq)]
pub struct AstTree<TNode> {
  /// Source file reference with filename and source text
  pub source: Arc<SourceFile>,
  /// File root
  pub nodes: Rc<TNode>,
}

impl<TNode> AstTree<TNode> {
  /// Creates a new AST tree from forms root
  pub fn new(source_file: Arc<SourceFile>, forms: Rc<TNode>) -> Self {
    Self {
      source: source_file,
      nodes: forms,
    }
  }
}

/// Parsed AST tree cache
pub struct AstCache<TNode> {
  /// AST trees keyed by filename
  pub items: HashMap<PathBuf, Arc<AstTree<TNode>>>,
}

impl<TNode> AstCache<TNode> {
  /// Create a new empty AST cache
  pub fn new_empty() -> Self {
    Self { items: Default::default() }
  }
}
