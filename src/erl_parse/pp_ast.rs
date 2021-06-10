use crate::erl_parse::Span;
use std::fmt::Debug;
use std::fmt;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};
use crate::types::ArcRw;
use crate::project::ErlProject;

/// While preprocessing source, the text is parsed into these segments
/// We are only interested in attributes (macros, conditionals, etc), macro pastes via ?MACRO and
/// comments where macros cannot occur. The rest of the text is parsed unchanged into tokens.
pub enum PpAstNode {
  /// A % line comment
  Comment(String),
  /// Any text
  Text(String),
  /// Any attribute without parentheses, -else. or -endif.
  Attr0(String),
  /// Any attribute even with 0 args, -if(), -define(NAME, xxxxx)
  Attr(String, Vec<String>),
  /// Paste macro tokens/text as is, use as: ?NAME
  PasteMacro(String, Vec<String>),
  /// Paste macro arguments as is, use as: ??PARAM
  PasteMacroAsString(String, Vec<String>),
  /// Included file from HRL cache
  IncludedFile(ArcRw<Vec<PpAstNode>>),
}

impl PpAstNode {
  pub fn trim(s: &String) -> String {
    const CLAMP_LENGTH: usize = 20;
    let trimmed = s.trim();
    if trimmed.len() <= CLAMP_LENGTH { return String::from(trimmed); }
    return String::from(&trimmed[..CLAMP_LENGTH - 1]) + "…";
  }
}

impl Debug for PpAstNode {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Comment(s) => write!(f, "%({})", Self::trim(s)),
      Self::Text(s) => write!(f, "T({})", Self::trim(s)),
      Self::Attr0(a) => write!(f, "Attr0({})", a),
      Self::Attr(a, args) => write!(f, "Attr({}, {:?})", a, args),
      Self::PasteMacro(_, _) => write!(f, "?M"),
      Self::PasteMacroAsString(_, _) => write!(f, "??M")
    }
  }
}

/// Stores HRL files parsed into PpAst tokens ready to be included into other files.
pub struct PpAstCache {
  pub syntax_trees: HashMap<PathBuf, ArcRw<Vec<PpAstNode>>>,
}

impl PpAstCache {
  pub fn new() -> Self {
    Self { syntax_trees: HashMap::with_capacity(ErlProject::DEFAULT_CAPACITY / 4) }
  }
}
