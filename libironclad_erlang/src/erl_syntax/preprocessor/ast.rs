//! Preprocessor subnode, stored in `ErlAst::Preprocessor()` enum variant

use crate::erl_syntax::erl_ast::ErlAst;
use crate::erl_syntax::preprocessor::macro_string::MacroString;
use std::path::PathBuf;
use std::sync::Arc;

/// Preprocessor syntax tree nodes, are stored in `ErlAst::Preprocessor()` enum variant
#[derive(Debug, Clone)]
pub enum PreprocessorNodeType {
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
    cond_true: Vec<Arc<ErlAst>>,
    /// The nested lines for the else block (if it was present)
    cond_false: Vec<Arc<ErlAst>>,
  },
  /// If(expression) stores an expression which must resolve to a constant value otherwise compile
  /// error will be triggered.
  IfBlock {
    /// The condition to check
    cond: Arc<ErlAst>,
    /// The nested lines
    cond_true: Vec<Arc<ErlAst>>,
    /// The nested lines for the else block (if it was present)
    cond_false: Vec<Arc<ErlAst>>,
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
    ast: Arc<ErlAst>,
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
