//! Defines AST structure for Erlang Preprocessor
use std::collections::HashMap;
use std::fmt::{Debug};
use std::path::PathBuf;
use std::sync::Arc;

use crate::erlang::syntax_tree::erl_ast::ErlAst;

/// While preprocessing source, the text is parsed into these segments
/// We are only interested in attributes (macros, conditionals, etc), macro pastes via ?MACRO and
/// comments where macros cannot occur. The rest of the text is parsed unchanged into tokens.
/// Lifetime note: Parse input string must live at least as long as this is alive
#[derive(Debug, Clone)]
pub enum PpAst {
  /// Default value for an empty AST tree
  Empty,

  /// Root of a preprocessed file
  File(Vec<Arc<PpAst>>),

  /// A % line comment
  Comment(String),

  /// Any text
  Text(String),

  /// Text("") shortcut
  EmptyText,

  /// Specific directive: -include("path").
  Include(String),

  /// Specific directive: -include_lib("path").
  IncludeLib(String),

  /// Define directive: `-define(NAME)` or `-define(NAME, TEXT)` or `-define(NAME(ARGS), TEXT)`.
  Define {
    /// Macro name
    name: String,
    /// Args if specified, different arity macros do not conflict each with other
    args: Option<Vec<String>>,
    /// Body if specified, any tokens, but since we have no tokenizer - any text
    body: Option<String>,
  },

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

  /// If(expression) stores an expression which must resolve to a constant value otherwise compile
  /// error will be triggered.
  If(Arc<ErlAst>),
  /// Elseif(expression)
  Elif(Arc<ErlAst>),

  /// Else clause of a conditional block
  Else,
  /// End of a conditional block
  Endif,

  /// Produce a compiler error
  Error(String),
  /// Produce a compiler warning
  Warning(String),

  /// Nested included file
  IncludedFile {
    /// Filename for this included file
    filename: PathBuf,
    /// Preprocessor sub-tree to descend into the includefile
    nested: Arc<PpAst>,
  },
}

impl PpAst {
  /// Trim the contents to CLAMP_LENGTH characters for convenient narrow debug printing
  pub fn trim(s: &str) -> &str {
    let trimmed = s.trim();
    &trimmed[..usize::min(trimmed.len(), 40) - 1]
  }
}

/// Parsed preprocessor AST cache
#[derive(Default)]
pub struct PpAstCache {
  /// AST trees keyed by filename
  pub items: HashMap<PathBuf, Arc<PpAst>>,
}
