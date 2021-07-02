use crate::erl_parse::Span;
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
  File(Vec<PpAstNode>),

  /// A % line comment
  Comment(Span),
  /// Any text
  Text(String),

  /// Generic attribute -name(args, ...).
  Attr { name: String, args: Vec<PpAstNode> },

  /// Specific directive: -module('atom').
  Module(String),

  /// Specific directive: -include("path").
  Include(String),

  /// Specific directive: -include_lib("path").
  IncludeLib(String),

  /// Specific directive: -define(NAME, any text...).
  Define(String, String),

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

  pub fn fmt(&self, source_file: &SourceFile) -> String {
    match self {
      Self::Comment(s) => format!("%({})", Self::trim(s.text(source_file))),
      Self::Text(s) => format!("T({})", Self::trim(s)),
      Self::Attr { name, args } => {
        let args_str = args.iter()
            .map(|arg| format!("{:?}", arg))
            .collect::<Vec<String>>()
            .join(", ");
        format!("Attr({}, {})", name, args_str)
      },

      Self::IncludedFile(include_rc) => {
        format!("include<{}>", include_rc.source.file_name.display())
      }
      PpAstNode::Module(p) => format!("Module({})", p),
      PpAstNode::Include(p) => format!("Include({})", p),
      PpAstNode::IncludeLib(p) => format!("IncludeLib({})", p),
      PpAstNode::File(nodes) => format!("File{{{:?}}}", nodes),
      PpAstNode::Define(name, body) => format!("Define({}, {})", name, body)
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
