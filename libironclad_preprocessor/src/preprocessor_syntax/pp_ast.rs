//! Defines AST structure for Erlang Preprocessor
use std::collections::HashMap;
use std::fmt::Debug;
use std::path::PathBuf;
use std::sync::Arc;

use crate::preprocessor_syntax::pp_macro_string::MacroString;
use libironclad_erlang::erl_syntax::erl_ast::ErlAst;
use libironclad_error::source_loc::SourceLoc;

/// While preprocessing source, the text is parsed into these segments
/// We are only interested in attributes (macros, conditionals, etc), macro pastes via ?MACRO and
/// comments where macros cannot occur. The rest of the text is parsed unchanged into tokens.
/// Lifetime note: Parse input string must live at least as long as this is alive
#[derive(Debug, Clone)]
pub struct PpAst {
  /// The node location in source
  pub location: SourceLoc,
  /// The node type and optional content
  pub node_type: PpAstType,
}

/// Type of a Preprocessor AST node
#[derive(Debug, Clone)]
pub enum PpAstType {
  /// Root of a preprocessed file
  File(Vec<Arc<PpAst>>),
  /// Any text. `Cell` in `MacroString` will allow replacing string with substituted macro values
  Text(MacroString),
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
    args: Vec<String>,
    /// Body if specified, any tokens, but since we have no tokenizer - any text
    body: MacroString,
  },
  /// Specific directive: -undef(NAME). removes a named macro definition
  Undef(String),
  /// Proceed interpreting AST nodes if the named macro is defined
  IfdefBlock {
    /// The condition to check
    macro_name: String,
    /// The nested lines
    cond_true: Vec<Arc<PpAst>>,
    /// The nested lines for the else block (if it was present)
    cond_false: Vec<Arc<PpAst>>,
  },
  /// If(expression) stores an expression which must resolve to a constant value otherwise compile
  /// error will be triggered.
  IfBlock {
    /// The condition to check
    cond: Arc<ErlAst>,
    /// The nested lines
    cond_true: Vec<Arc<PpAst>>,
    /// The nested lines for the else block (if it was present)
    cond_false: Vec<Arc<PpAst>>,
  },
  /// Produce a libironclad error
  Error(String),
  /// Produce a libironclad warning
  Warning(String),
  /// Nested included file
  IncludedFile {
    /// Filename for this included file
    filename: PathBuf,
    /// Preprocessor sub-tree to descend into the includefile
    ast: Arc<PpAst>,
  },
  // Temporary nodes, appear during parsing and should never appear into the final AST output.
  // These values never leave the parser module.
  /// `-else.` node
  _TemporaryElse,
  ///`-endif` node
  _TemporaryEndif,
  /// `-if(...).` node
  _TemporaryIf(Arc<ErlAst>),
  /// `-elif(...).` node
  _TemporaryElseIf(Arc<ErlAst>),
  /// -ifdef(...). is translated into `IfdefBlock`
  _TemporaryIfdef(String),
  /// -ifndef(...). is translated into `IfdefBlock`
  _TemporaryIfndef(String),
}

impl PpAst {
  /// Trim the contents to CLAMP_LENGTH characters for convenient narrow debug printing
  #[allow(dead_code)]
  fn trim(s: &str) -> &str {
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
