use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::Arc;

use crate::project::ErlProject;
use crate::syntaxtree::ast_cache::{AstCache, AstTree};
use std::rc::Rc;

/// While preprocessing source, the text is parsed into these segments
/// We are only interested in attributes (macros, conditionals, etc), macro pastes via ?MACRO and
/// comments where macros cannot occur. The rest of the text is parsed unchanged into tokens.
/// Lifetime note: Parse input string must live at least as long as this is alive
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PpAst {
  /// Default value for an empty AST tree
  Empty,

  // /// Do nothing node, deleted code
  // Skip,

  File(Vec<Rc<PpAst>>),

  /// A % line comment
  Comment(String),

  /// Any text
  Text(String),

  /// Specific directive: -include("path").
  Include(String),

  /// Specific directive: -include_lib("path").
  IncludeLib(String),

  /// Specific directive: -define(NAME, any text...).
  Define(String, String),
  DefineFun { name: String, args: Vec<String>, body: String },

  /// Specific directive: -undef(NAME).
  Undef(String),

  Ifdef(String),
  Ifndef(String),

  /// If and Elif store Erlang syntax parsable by Erlang grammar, which must resolve to a constant
  /// expression otherwise compile error will be triggered.
  If(String),
  Elif(String),

  Else,
  Endif,

  Error(String),
  Warning(String),

  IncludedFile(Arc<PpAstTree>),
}

impl PpAst {
  pub fn trim(s: &str) -> &str {
    const CLAMP_LENGTH: usize = 40;
    let trimmed = s.trim();
    if trimmed.len() <= CLAMP_LENGTH {
      return trimmed;
    }
    &trimmed[..CLAMP_LENGTH - 1]
  }

  pub fn to_dbg_str(&self) -> String {
    match self {
      Self::Comment(s) => format!("Comment({})", Self::trim(s)),
      Self::Text(s) => format!("T({})", Self::trim(s)),

      Self::IncludedFile(include_rc) => {
        format!("include<{}>", include_rc.source.file_name.display())
      }
      PpAst::Include(p) => format!("Include({})", p),
      PpAst::IncludeLib(p) => format!("IncludeLib({})", p),
      PpAst::File(nodes) => format!("File{{{:?}}}", nodes),
      PpAst::Define(name, body) => format!("Define({}, {})", name, body),
      PpAst::DefineFun { name, args, body } => format!("Define({}({:?}), {})", name, args, body),
      PpAst::Ifdef(name) => format!("If Def({})", name),
      PpAst::Ifndef(name) => format!("If !Def({})", name),
      PpAst::Else => "Else".to_string(),
      PpAst::Endif => "Endif".to_string(),
      PpAst::If(expr) => format!("If({})", expr),
      PpAst::Elif(expr) => format!("Elif({})", expr),
      PpAst::Undef(name) => format!("Undef({})", name),
      PpAst::Error(t) => format!("Error({})", t),
      PpAst::Warning(t) => format!("Warning({})", t),
      PpAst::Empty => unreachable!("PpAst::Empty encountered, while it shouldn't"),
    }
  }

  pub fn to_string(&self) -> String {
    match self {
      PpAst::File(items) => {
        items.into_iter()
            .map(|node| node.to_string())
            .collect::<Vec<String>>()
            .join("\n")
      }
      PpAst::Text(s) => s.clone(),
      PpAst::IncludedFile(include_rc) => include_rc.nodes.to_string(),
      PpAst::Define(name, body) => format!("-define({}, {}).", name, body),
      PpAst::DefineFun { name, args, body } => {
        format!("-define({}({:?}), {})", name, args, body)
      },
      PpAst::Ifdef(name) => format!("-ifdef({}).", name),
      PpAst::Ifndef(name) => format!("-ifndef({}).", name),
      PpAst::Else => "-else.".to_string(),
      PpAst::Endif => "-endif.".to_string(),
      PpAst::If(expr) => format!("-if({}).", expr),
      PpAst::Elif(expr) => format!("-elif({}).", expr),
      PpAst::Undef(name) => format!("-undef({}).", name),
      PpAst::Error(t) => format!("-error({}).", t),
      PpAst::Warning(t) => format!("-warning({}).", t),

      _ => unreachable!("PpAst::to_string() can't process {:?}", self),
    }
  }
}


/// A tree of Preprocessor syntax nodes with attached file name, and root element removed
pub(crate) type PpAstTree = AstTree<PpAst>;

/// A cache of trees of Preprocessor syntax nodes, keyed by filename or module name
pub(crate) type PpAstCache = AstCache<PpAst>;

impl PpAstCache {
  pub fn new() -> Self {
    Self {
      items: HashMap::with_capacity(ErlProject::DEFAULT_CAPACITY / 4),
    }
  }
}
