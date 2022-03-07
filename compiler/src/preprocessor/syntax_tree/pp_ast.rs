//! Defines AST structure for Erlang Preprocessor
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::sync::Arc;
use ::function_name::named;

use crate::project::ErlProject;
use crate::ast_tree::{AstCache, AstTree};
use std::rc::Rc;

/// While preprocessing source, the text is parsed into these segments
/// We are only interested in attributes (macros, conditionals, etc), macro pastes via ?MACRO and
/// comments where macros cannot occur. The rest of the text is parsed unchanged into tokens.
/// Lifetime note: Parse input string must live at least as long as this is alive
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PpAst {
  /// Default value for an empty AST tree
  Empty,

  /// Root of a preprocessed file
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
  /// Defines a macro with parameters, and body
  DefineFun {
    /// Name of the macro
    name: String,
    /// Arguments as strings
    args: Vec<String>,
    /// Macro body
    body: String,
  },

  /// Specific directive: -undef(NAME). removes a named macro definition
  Undef(String),

  /// Proceed interpreting AST nodes if the named macro is defined
  Ifdef(String),
  /// ...or not defined
  Ifndef(String),

  /// If and Elif store Erlang syntax parsable by Erlang grammar, which must resolve to a constant
  /// expression otherwise compile error will be triggered.
  If(String),
  /// Else if
  Elif(String),

  /// Else clause of a conditional block
  Else,
  /// End of a conditional block
  Endif,

  /// Produce a compiler error
  Error(String),
  /// Produce a compiler warning
  Warning(String),

  /// Nested included file
  IncludedFile(Arc<PpAstTree>),
}

impl PpAst {
  /// Trim the contents to CLAMP_LENGTH characters for convenient narrow debug printing
  pub fn trim(s: &str) -> &str {
    const CLAMP_LENGTH: usize = 40;
    let trimmed = s.trim();
    if trimmed.len() <= CLAMP_LENGTH {
      return trimmed;
    }
    &trimmed[..CLAMP_LENGTH - 1]
  }

  /// Format as a debug string
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
}

impl std::fmt::Display for PpAst {
  /// Format AST as a string
  #[named]
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      PpAst::File(items) => {
        for i in items.iter() {
          writeln!(f, "{}", i)?;
        }
        Ok(())
      }
      PpAst::Text(s) => write!(f, "{}", s),
      PpAst::IncludedFile(include_rc) => write!(f, "{}", include_rc.nodes),
      PpAst::Define(name, body) => write!(f, "-define({}, {}).", name, body),
      PpAst::DefineFun { name, args, body } => {
        write!(f, "-define({}({:?}), {})", name, args, body)
      }
      PpAst::Ifdef(name) => write!(f, "-ifdef({}).", name),
      PpAst::Ifndef(name) => write!(f, "-ifndef({}).", name),
      PpAst::Else => write!(f, "-else."),
      PpAst::Endif => write!(f, "-endif."),
      PpAst::If(expr) => write!(f, "-if({}).", expr),
      PpAst::Elif(expr) => write!(f, "-elif({}).", expr),
      PpAst::Undef(name) => write!(f, "-undef({}).", name),
      PpAst::Error(t) => write!(f, "-error({}).", t),
      PpAst::Warning(t) => write!(f, "-warning({}).", t),
      PpAst::Comment(t) => write!(f, "% {}", t),

      _ => unreachable!("{}(): can't process {:?}", function_name!(), self),
    }
  }
}


/// A tree of Preprocessor syntax nodes with attached file name, and root element removed
pub type PpAstTree = AstTree<PpAst>;

/// A cache of trees of Preprocessor syntax nodes, keyed by filename or module name
pub type PpAstCache = AstCache<PpAst>;

impl Default for PpAstCache {
  /// Create a new empty AST cache for preprocessed files
  fn default() -> Self {
    Self {
      items: HashMap::with_capacity(ErlProject::DEFAULT_CAPACITY / 4),
    }
  }
}
