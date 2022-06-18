//! Preprocessor subnode, stored in `ErlAst::Preprocessor()` enum variant

use crate::erl_syntax::erl_ast::AstNode;
use std::path::PathBuf;

/// Preprocessor syntax tree nodes, are stored in `ErlAst::Preprocessor()` enum variant
#[derive(Debug, Clone)]
#[deprecated = "preprocessor does not belong in AST"]
pub enum PreprocessorNodeType {
  /// Specific directive: -include("path").
  #[deprecated = "includes are executed while parsing"]
  Include(String),
  /// Specific directive: -include_lib("path").
  #[deprecated = "includes are executed while parsing"]
  IncludeLib(String),
  /// Define directive: `-define(NAME)` or `-define(NAME, TEXT)` or `-define(NAME(ARGS), TEXT)`.
  #[deprecated = "defines are executed while parsing"]
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
  #[deprecated = "undefs are executed while parsing"]
  Undef(String),
  /// Proceed interpreting AST nodes if the named macro is defined
  #[deprecated = "conditions are executed while parsing"]
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
  #[deprecated = "conditions are executed while parsing"]
  IfBlock {
    /// The condition to check
    cond: AstNode,
    /// The nested lines
    cond_true: Vec<AstNode>,
    /// The nested lines for the else block (if it was present)
    cond_false: Vec<AstNode>,
  },
  /// Produce a libironclad error
  #[deprecated = "preprocessor stage is not in ast"]
  Error(String),
  /// Produce a libironclad warning
  #[deprecated = "preprocessor stage is not in ast"]
  Warning(String),
  /// Nested included file
  #[deprecated = "preprocessor stage is not in ast"]
  IncludedFile {
    /// Filename for this included file
    filename: PathBuf,
    /// Preprocessor sub-tree to descend into the includefile
    ast: AstNode,
  },
  // Temporary nodes, appear during parsing and should never appear into the final AST output.
  // These values never leave the parser module.
  /// `-else.` node
  #[deprecated = "preprocessor stage is not in ast"]
  _TemporaryElse,
  ///`-endif` node
  #[deprecated = "preprocessor stage is not in ast"]
  _TemporaryEndif,
  /// `-if(...).` node
  #[deprecated = "preprocessor stage is not in ast"]
  _TemporaryIf(AstNode),
  /// `-elif(...).` node
  #[deprecated = "preprocessor stage is not in ast"]
  _TemporaryElseIf(AstNode),
  /// -ifdef(...). is pushed on conditions stack in `ParserScope` during parsing
  #[deprecated = "preprocessor stage is not in ast"]
  _TemporaryIfdef(String),
  /// -ifndef(...). is translated into `IfdefBlock`
  #[deprecated = "preprocessor stage is not in ast"]
  _TemporaryIfndef(String),
  /// Holds true branches of -if/-ifdef directives, and must be unfolded after parsing. Can only
  /// occur on top level with other module forms, and is flattened automatically on creating the
  /// module root node.
  #[deprecated = "preprocessor stage is not in ast"]
  Group(Vec<AstNode>),
}
