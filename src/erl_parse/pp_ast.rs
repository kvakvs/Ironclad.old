use std::fmt::Debug;
use std::fmt;
use std::collections::HashMap;
use std::path::PathBuf;
use crate::types::ArcRw;
use crate::project::ErlProject;

/// While preprocessing source, the text is parsed into these segments
/// We are only interested in attributes (macros, conditionals, etc), macro pastes via ?MACRO and
/// comments where macros cannot occur. The rest of the text is parsed unchanged into tokens.
/// Lifetime note: Parse input string must live at least as long as this is alive
#[derive(Clone)]
pub enum PpAstNode<'a> {
  /// A % line comment
  Comment(&'a str),
  /// Any text
  Text(&'a str),
  /// Any attribute even with 0 args, -if(), -define(NAME, xxxxx)
  Attr { name: &'a str, body: Option<&'a str> },
  /// Paste macro tokens/text as is, use as: ?NAME
  PasteMacro { name: &'a str, body: Option<&'a str> },
  /// Paste macro arguments as is, use as: ??PARAM
  StringifyMacroParam { name: &'a str },
  /// Included file from HRL cache
  IncludedFile(ArcRw<PpAstTree<'a>>),
}

impl<'a> PpAstNode<'a> {
  pub fn trim(s: &'a str) -> &'a str {
    const CLAMP_LENGTH: usize = 40;
    let trimmed = s.trim();
    if trimmed.len() <= CLAMP_LENGTH { return trimmed; }
    &trimmed[..CLAMP_LENGTH - 1]
  }
}

impl<'a> Debug for PpAstNode<'a> {
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

/// Lifetime note: Parse input string must live at least as long as this is alive
pub struct PpAstTree<'a> {
  /// Clone of filename where this was loaded from
  pub file_name: PathBuf,
  /// The parsed preprocessor syntax tree ready for inclusion
  pub nodes: Vec<PpAstNode<'a>>,
}

impl<'a> PpAstTree<'a> {
  pub fn new(file_name: PathBuf, nodes: Vec<PpAstNode<'a>>) -> Self {
    PpAstTree { file_name, nodes }
  }
}

/// Stores HRL files parsed into PpAst tokens ready to be included into other files.
/// Lifetime note: Cache must live at least as long as parse trees are alive
pub struct PpAstCache<'a> {
  pub syntax_trees: HashMap<PathBuf, ArcRw<PpAstTree<'a>>>,
}

impl<'a> PpAstCache<'a> {
  pub fn new() -> Self {
    Self { syntax_trees: HashMap::with_capacity(ErlProject::DEFAULT_CAPACITY / 4) }
  }
}
