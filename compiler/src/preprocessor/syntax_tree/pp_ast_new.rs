//! Construction routines for PpAst

use std::path::{Path, PathBuf};
use std::sync::Arc;
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::preprocessor::syntax_tree::pp_ast::PpAst;

impl PpAst {
  /// Create new file tree
  pub fn new_file(fragments: Vec<Arc<PpAst>>) -> Arc<Self> {
    PpAst::File(fragments).into()
  }

  /// Create new nested included file AST node
  pub fn new_included_file(file: &Path, ast: Arc<PpAst>) -> Arc<Self> {
    PpAst::IncludedFile {
      filename: PathBuf::from(file),
      nested: ast,
    }.into()
  }

  /// Create new macro definition
  pub fn new_define(name: String, args: Option<Vec<String>>, body: Option<String>) -> Arc<Self> {
    PpAst::Define { name, args, body }.into()
  }

  /// Create new text fragment
  pub fn new_text(text: &str) -> Arc<Self> {
    PpAst::Text(text.into()).into()
  }

  /// Creates a new preprocessor IF node
  pub fn new_if(expr: Arc<ErlAst>) -> Arc<Self> {
    PpAst::If(expr).into()
  }

  /// Creates a new preprocessor ELSEIF node
  pub fn new_elseif(expr: Arc<ErlAst>) -> Arc<Self> {
    PpAst::Elif(expr).into()
  }
}