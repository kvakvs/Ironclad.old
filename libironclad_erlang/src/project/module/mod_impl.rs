//! Implementation for `ErlModule`

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::erl_error::ErlError;
use crate::erl_syntax::parsers::misc::panicking_tokenizer_error_reporter;
use crate::erl_syntax::parsers::parser_scope::{ParserScope, ParserScopeImpl};
use crate::erl_syntax::token_stream::tok_input::{TokenizerInput, TokensResult};
use crate::erl_syntax::token_stream::token::Token;
use crate::error::ic_error::IcResult;
use crate::project::compiler_opts::{CompilerOpts, CompilerOptsImpl};
use crate::project::module::scope::root_scope::RootScope;
use crate::project::ErlProject;
use crate::source_file::{SourceFile, SourceFileImpl};
use nom::Finish;
use std::cell::RefCell;
use std::fmt;
use std::fmt::Debug;
use std::sync::{Arc, RwLock};

/// Erlang Module consists of
/// - List of forms: attributes, and Erlang functions
/// - Compiler options used to produce this module
pub struct ErlModuleImpl {
  /// Options used to build this module. Possibly just a ref to the main project's options
  pub compiler_options: CompilerOpts,
  /// Module name atom, as a string
  pub name: RefCell<String>,
  /// The file we're processing AND the file contents (owned by SourceFile)
  pub source_file: SourceFile,
  /// Stores state of preprocessor defines during parse, and then final state after parse (useful for testing)
  pub parser_scope: ParserScope,
  /// AST tree of the module.
  pub ast: RefCell<AstNode>,
  // /// Local level scope, containing variables
  // pub scope: Scope,
  /// Module-level scope with types, functions, and other global stuff
  pub root_scope: RootScope,
  /// Accumulates found errors in this module. Tries to hard break the operations when error limit
  /// is reached.
  pub errors: RwLock<Vec<ErlError>>,
}

/// Wraps module into runtime-lockable refcount
pub type ErlModule = Arc<ErlModuleImpl>;

impl Default for ErlModuleImpl {
  fn default() -> Self {
    Self {
      compiler_options: Default::default(),
      name: RefCell::new(String::default()),
      source_file: Arc::new(SourceFileImpl::default()),
      parser_scope: ParserScopeImpl::new_empty().into(),
      ast: RefCell::new(AstNodeImpl::new_empty("dummy node for module root".to_string())),
      root_scope: Default::default(),
      errors: RwLock::new(Vec::with_capacity(CompilerOptsImpl::MAX_ERRORS_PER_MODULE * 110 / 100)),
    }
  }
}

impl Debug for ErlModuleImpl {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "ErlModule({})", self.name.borrow())
  }
}

impl ErlModuleImpl {
  /// Create an empty and wrap with `Arc<Rwlock<>>`
  pub fn new_default() -> ErlModule {
    Self::default().into()
  }

  /// Create a new empty module
  pub fn new(opt: CompilerOpts, source_file: SourceFile) -> ErlModule {
    Self {
      compiler_options: opt,
      source_file,
      ..Default::default()
    }
    .into()
  }

  /// Generic tokenizer for any Nom entry point
  pub fn tokenize_helper<T>(
    _project: &ErlProject,
    src_file: SourceFile,
    parse_fn: T,
  ) -> IcResult<Vec<Token>>
  where
    T: Fn(TokenizerInput) -> TokensResult<Vec<Token>>,
  {
    let input = src_file.text.as_str();
    let forms = panicking_tokenizer_error_reporter(input, parse_fn(input).finish());

    Ok(forms)
  }

  /// Adds an error to vector of errors. Returns false when error list is full and the calling code
  /// should attempt to stop.
  pub fn add_error(&self, err: ErlError) -> bool {
    if let Ok(mut w_errors) = self.errors.write() {
      w_errors.push(err);
      w_errors.len() < self.compiler_options.max_errors_per_module
    } else {
      panic!("Can't lock module errors for appending")
    }
  }

  /// Update the module name when we learn it from -module() attribute
  pub fn set_name(&self, name: &str) {
    assert!(self.name.borrow().is_empty()); // can only update once
    self.name.replace(name.to_string());
  }
}
