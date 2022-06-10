//! Preprocessor subnode, stored in `ErlAst::Preprocessor()` enum variant

use crate::erl_syntax::erl_ast::AstNode;
use std::path::PathBuf;

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
    body: String,
  },
  // MacroInvocation0 {
  //   /// Macro identifier in `? IDENT`
  //   name: String,
  // },
  // MacroInvocation {
  //   /// Macro identifier in `? IDENT ( ARGS )`
  //   name: String,
  //   /// Args parsed to Erlang AST subtrees (parser does not support all possible AST nodes everywhere)
  //   args: Vec<AstNode>,
  // },
  /// Specific directive: -undef(NAME). removes a named macro definition
  Undef(String),
  /// Proceed interpreting AST nodes if the named macro is defined
  IfdefBlock {
    /// The condition to check
    macro_name: String,
    /// The nested lines
    cond_true: Vec<AstNode>,
    /// The nested lines for the else block (if it was present)
    cond_false: Vec<AstNode>,
  },
  /// If(expression) stores an expression which must resolve to a constant value otherwise compile
  /// error will be triggered.
  IfBlock {
    /// The condition to check
    cond: AstNode,
    /// The nested lines
    cond_true: Vec<AstNode>,
    /// The nested lines for the else block (if it was present)
    cond_false: Vec<AstNode>,
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
    ast: AstNode,
  },
  // Temporary nodes, appear during parsing and should never appear into the final AST output.
  // These values never leave the parser module.
  /// `-else.` node
  _TemporaryElse,
  ///`-endif` node
  _TemporaryEndif,
  /// `-if(...).` node
  _TemporaryIf(AstNode),
  /// `-elif(...).` node
  _TemporaryElseIf(AstNode),
  /// -ifdef(...). is translated into `IfdefBlock`
  _TemporaryIfdef(String),
  /// -ifndef(...). is translated into `IfdefBlock`
  _TemporaryIfndef(String),
}
