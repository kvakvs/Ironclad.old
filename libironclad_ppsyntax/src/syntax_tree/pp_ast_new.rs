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
      ast: ast,
    }.into()
  }

  /// Create new macro definition
  pub fn new_define(name: String, args: Option<Vec<String>>, body: Option<String>) -> Arc<Self> {
    PpAst::Define { name, args, body }.into()
  }

  /// Create new text fragment
  pub fn new_text(text: &str) -> Arc<Self> {
    if text.trim().is_empty() {
      PpAst::EmptyText
    } else {
      PpAst::Text(text.into())
    }.into()
  }

  /// Creates a new preprocessor IF node
  pub fn new_if(expr: Arc<ErlAst>,
                cond_true: Option<Vec<Arc<PpAst>>>,
                cond_false: Option<Vec<Arc<PpAst>>>) -> Arc<Self> {
    PpAst::IfBlock {
      cond: expr,
      cond_true,
      cond_false,
    }.into()
  }

  /// Create a new `-if()` temporary node.
  pub fn new_if_temporary(expr: Arc<ErlAst>) -> Arc<Self> {
    PpAst::_TemporaryIf(expr).into()
  }

  /// Create a new `-elif()` temporary node.
  pub fn new_elif_temporary(expr: Arc<ErlAst>) -> Arc<Self> {
    PpAst::_TemporaryElseIf(expr).into()
  }

  /// Create a new `-ifdef()` temporary node.
  pub fn new_ifdef_temporary(ident: String) -> Arc<Self> {
    PpAst::_TemporaryIfdef(ident).into()
  }

  /// Create a new `-ifndef()` temporary node.
  pub fn new_ifndef_temporary(ident: String) -> Arc<Self> {
    PpAst::_TemporaryIfndef(ident).into()
  }

  /// Create a new UNDEF node
  pub fn new_undef(ident: String) -> Arc<Self> { PpAst::Undef(ident).into() }

  /// Create a new INCLUDE node
  pub fn new_include(p: String) -> Arc<Self> { PpAst::Include(p).into() }

  /// Create a new INCLUDE_LIB node
  pub fn new_include_lib(p: String) -> Arc<Self> { PpAst::IncludeLib(p).into() }
}