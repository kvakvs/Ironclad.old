use std::fmt::Debug;
use std::fmt;
use std::collections::HashMap;
use std::path::PathBuf;
use crate::types::ArcRw;
use crate::project::ErlProject;

/// While preprocessing source, the text is parsed into these segments
/// We are only interested in attributes (macros, conditionals, etc), macro pastes via ?MACRO and
/// comments where macros cannot occur. The rest of the text is parsed unchanged into tokens.
#[derive(Clone)]
pub enum PpAstNode {
  /// A % line comment
  Comment(String),
  /// Any text
  Text(String),
  /// Any attribute even with 0 args, -if(), -define(NAME, xxxxx)
  Attr { name: String, body: Option<String> },
  /// Paste macro tokens/text as is, use as: ?NAME
  PasteMacro { name: String, body: Option<String> },
  /// Paste macro arguments as is, use as: ??PARAM
  StringifyMacroParam { name: String },
  /// Included file from HRL cache
  IncludedFile(ArcRw<PpAstTree>),
}

impl PpAstNode {
  pub fn trim(s: &str) -> String {
    const CLAMP_LENGTH: usize = 20;
    let trimmed = s.trim();
    if trimmed.len() <= CLAMP_LENGTH { return String::from(trimmed); }
    String::from(&trimmed[..CLAMP_LENGTH - 1]) + "…"
  }
}

impl Debug for PpAstNode {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Comment(s) => write!(f, "%({})", Self::trim(s)),
      Self::Text(s) => write!(f, "T({})", Self::trim(s)),
      Self::Attr { name, body } => write!(f, "Attr({}, {:?})", name, body),
      Self::PasteMacro { name, body } => write!(f, "?{}({:?})", name, body),
      Self::StringifyMacroParam { name } => write!(f, "??{}", name),
      Self::IncludedFile(include_rwlock) => {
        let ast_r = include_rwlock.read().unwrap();
        let result = write!(f, "include<{}>", ast_r.file_name.display());
        drop(ast_r);
        result
      }
    }
  }
}

pub struct PpAstTree {
  /// Clone of filename where this was loaded from
  pub file_name: PathBuf,
  /// The parsed preprocessor syntax tree ready for inclusion
  pub nodes: Vec<PpAstNode>,
}

impl PpAstTree {
  pub fn new(file_name: PathBuf, nodes: Vec<PpAstNode>) -> Self {
    PpAstTree { file_name, nodes }
  }
}

/// Stores HRL files parsed into PpAst tokens ready to be included into other files.
pub struct PpAstCache {
  pub syntax_trees: HashMap<PathBuf, ArcRw<PpAstTree>>,
}

impl PpAstCache {
  pub fn new() -> Self {
    Self { syntax_trees: HashMap::with_capacity(ErlProject::DEFAULT_CAPACITY / 4) }
  }
}
