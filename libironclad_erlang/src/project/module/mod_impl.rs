//! Implementation for `ErlModule`

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::erl_error::ErlError;
use crate::erl_syntax::parsers::misc::panicking_tokenizer_error_reporter;
use crate::erl_syntax::token_stream::tok_input::{TokenizerInput, TokensResult};
use crate::erl_syntax::token_stream::token::Token;
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::erl_syntax::token_stream::tokenizer::tokenize_source;
use crate::error::ic_error::IcResult;
use crate::project::compiler_opts::{CompilerOpts, CompilerOptsImpl};
use crate::project::module::scope::root_scope::RootScope;
use crate::project::ErlProject;
use libironclad_util::rw_vec::RwVec;
use libironclad_util::source_file::{SourceFile, SourceFileImpl};
use nom::Finish;
use std::cell::RefCell;
use std::fmt;
use std::fmt::Debug;
use std::ptr::null;
use std::sync::{Arc, RwLock};

/// Erlang Module consists of
/// - List of forms: attributes, and Erlang functions
/// - Compiler options used to produce this module
pub struct ErlModuleImpl {
  /// Options used to build this module. Possibly just a ref to the main project's options
  pub compiler_options: CompilerOpts,
  /// Module name atom, as a string
  pub name: RwLock<String>,
  /// The file we're processing AND the file contents (owned by SourceFile)
  pub source_file: SourceFile,
  /// AST tree of the module.
  pub ast: RefCell<AstNode>,
  // /// Local level scope, containing variables
  // pub scope: Scope,
  /// Module-level scope with types, functions, and other global stuff
  pub root_scope: RootScope,
  /// Accumulates found errors in this module. Tries to hard break the operations when error limit
  /// is reached.
  pub errors: RwVec<ErlError>,
  /// Warnings which can accumulate but do not block the processing
  pub warnings: RwVec<ErlError>,
  // /// Collection of included files to prevent circular imports
  // pub included_files: RwHashSet<OsString>,
}

/// Wraps module into runtime-lockable refcount
pub type ErlModule = Arc<ErlModuleImpl>;

impl Default for ErlModuleImpl {
  fn default() -> Self {
    Self {
      compiler_options: Default::default(),
      name: RwLock::new(String::default()),
      source_file: Arc::new(SourceFileImpl::default()),
      ast: RefCell::new(AstNodeImpl::new_empty("dummy node for module root".to_string())),
      root_scope: RootScope::default(),
      errors: RwVec::with_capacity(CompilerOptsImpl::MAX_ERRORS_PER_MODULE * 110 / 100),
      warnings: RwVec::default(),
      // included_files: Default::default(),
    }
  }
}

impl Debug for ErlModuleImpl {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "ErlModule({})", self.name.read().unwrap())
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

  /// Given project, module and the source file, break it into tokens, and interpret the
  /// preprocessor directives using module root scope.
  pub fn tokenize(
    project: &ErlProject,
    module: &ErlModule,
    src_file: &SourceFile,
  ) -> IcResult<Vec<Token>> {
    //----------------------
    // Stage 1 tokenize the input
    //----------------------
    let mut tok_stream1 =
      ErlModuleImpl::tokenize_helper(project, src_file.clone(), tokenize_source)?;

    // Inject a mandatory EOL if the stream doesn't end with one
    if !Token::ends_with(&tok_stream1, &[TokenType::EOL]) {
      tok_stream1.push(Token::new(null::<u8>(), TokenType::EOL));
    }

    //----------------------
    // Stage 2 preprocessor: handle ifdefs, defines, includes etc
    // tokenize includes and paste in the token stream too
    //----------------------
    let tok_stream2 = ErlModuleImpl::preprocess_interpret(src_file, project, module, tok_stream1)?;
    ErlModuleImpl::verify_integrity(&module)?;

    Ok(tok_stream2)
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
    self.errors.push(err);
    self.errors.len() < self.compiler_options.max_errors_per_module
  }

  /// Adds an warning to vector of warnings.
  pub fn add_warning(&self, err: ErlError) {
    self.warnings.push(err);
  }

  /// Update the module name when we learn it from -module() attribute
  pub fn set_name(&self, name: &str) {
    if cfg!(debug_assertions) {
      let self_name = self.name.read().unwrap();
      assert!(
        self_name.is_empty(),
        "Only one -module() attribute per module is allowed, the name is already set to {}",
        self_name
      );
    }
    if let Ok(mut w_name) = self.name.write() {
      w_name.clear();
      w_name.push_str(name);
    }
  }

  /// Access module name
  pub fn get_name(&self) -> String {
    self.name.read().unwrap().clone()
  }

  /// Check whether any errors were reported for this module
  pub fn has_errors(&self) -> bool {
    self.errors.len() > 0
  }

  /// Print errors accumulated for this module
  pub fn print_errors(&self) {
    if let Ok(r_errors) = self.errors.data.read() {
      for err in r_errors.iter() {
        println!("{}", err);
      }
    } else {
      panic!("Can't lock module errors collection for printing")
    }

    if let Ok(r_warnings) = self.warnings.data.read() {
      for wrn in r_warnings.iter() {
        println!("{}", wrn);
      }
    } else {
      panic!("Can't lock module warnings collection for printing")
    }
  }
}
