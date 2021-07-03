use crate::project::source_file::SourceFile;
use crate::project::ErlProject;
use std::collections::HashMap;
use std::fmt::Debug;
use std::path::PathBuf;
use std::sync::Arc;

/// While preprocessing source, the text is parsed into these segments
/// We are only interested in attributes (macros, conditionals, etc), macro pastes via ?MACRO and
/// comments where macros cannot occur. The rest of the text is parsed unchanged into tokens.
/// Lifetime note: Parse input string must live at least as long as this is alive
#[derive(Debug, Clone)]
pub enum PpAstNode {
  // /// Do nothing node, deleted code
  // Skip,

  File(Vec<PpAstNode>),

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

  /// Specific directive: -undef(NAME).
  Undef(String),

  Ifdef(String),
  Ifndef(String),
  If(String),
  Elif(String),
  Else,
  Endif,

  Error(String),
  Warning(String),

  IncludedFile(Arc<PpAstTree>),
}

impl PpAstNode {
  pub fn trim(s: &str) -> &str {
    const CLAMP_LENGTH: usize = 40;
    let trimmed = s.trim();
    if trimmed.len() <= CLAMP_LENGTH {
      return trimmed;
    }
    &trimmed[..CLAMP_LENGTH - 1]
  }

  pub fn fmt(&self) -> String {
    match self {
      Self::Comment(s) => format!("Comment({})", Self::trim(s)),
      Self::Text(s) => format!("T({})", Self::trim(s)),

      // Self::Attr { name, args } => {
      //   let args_str = args.iter()
      //       .map(|arg| format!("{:?}", arg))
      //       .collect::<Vec<String>>()
      //       .join(", ");
      //   format!("Attr({}, {})", name, args_str)
      // },

      Self::IncludedFile(include_rc) => {
        format!("include<{}>", include_rc.source.file_name.display())
      }
      PpAstNode::Include(p) => format!("Include({})", p),
      PpAstNode::IncludeLib(p) => format!("IncludeLib({})", p),
      PpAstNode::File(nodes) => format!("File{{{:?}}}", nodes),
      PpAstNode::Define(name, body) => format!("Define({}, {})", name, body),
      PpAstNode::Ifdef(name) => format!("If Def({})", name),
      PpAstNode::Ifndef(name) => format!("If !Def({})", name),
      PpAstNode::Else => format!("Else"),
      PpAstNode::Endif => format!("Endif"),
      PpAstNode::If(name) => format!("If({})", name),
      PpAstNode::Elif(name) => format!("Else If({})", name),
      PpAstNode::Undef(name) => format!("Undef({})", name),
      PpAstNode::Error(t) => format!("Error({})", t),
      PpAstNode::Warning(t) => format!("Warning({})", t),
    }
  }
}

/// Lifetime note: Parse input string must live at least as long as this is alive
#[derive(Debug)]
pub struct PpAstTree {
  pub source: Arc<SourceFile>,
  /// The parsed preprocessor syntax tree ready for inclusion
  pub nodes: Vec<PpAstNode>,
}

impl PpAstTree {
  /// Take ownership on source text
  pub fn new(source_file: Arc<SourceFile>, nodes: Vec<PpAstNode>) -> Self {
    PpAstTree {
      nodes,
      source: source_file,
    }
  }
}

/// Stores HRL files parsed into PpAst tokens ready to be included into other files.
/// Lifetime note: Cache must live at least as long as parse trees are alive
pub struct PpAstCache {
  pub syntax_trees: HashMap<PathBuf, Arc<PpAstTree>>,
}

impl PpAstCache {
  pub fn new() -> Self {
    Self {
      syntax_trees: HashMap::with_capacity(ErlProject::DEFAULT_CAPACITY / 4),
    }
  }
}
