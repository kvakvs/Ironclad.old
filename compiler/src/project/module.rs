//! Defines an Erlang module ready to be compiled
use std::cell::RefCell;
use std::fmt::Debug;
use std::fmt;
use std::path::{Path};
use std::sync::{Arc, RwLock};
use nom::Finish;

use crate::erl_error::{ErlError, ErlResult};
use crate::project::compiler_opts::CompilerOpts;
use crate::project::source_file::SourceFile;
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::nom_parse::{ErlParser, ErlParserError};
use crate::typing::scope::Scope;

/// Erlang Module consists of
/// - List of forms: attributes, and Erlang functions
/// - Compiler options used to produce this module
pub struct Module {
  /// Options used to build this module. Possibly just a ref to the main project's options
  pub compiler_options: Arc<CompilerOpts>,
  /// Module name atom, as a string
  pub name: String,
  /// The file we're processing AND the file contents (owned by SourceFile)
  pub source_file: Arc<SourceFile>,

  /// AST tree of the module.
  pub ast: Arc<ErlAst>,

  // /// Collection of module functions and a lookup table
  // pub registry: RwLock<FuncRegistry>,

  /// Module level scope, containing functions
  pub scope: Arc<RwLock<Scope>>,

  /// Accumulates found errors in this module. Tries to hard break the operations when error limit
  /// is reached.
  pub errors: RefCell<Vec<ErlError>>,
}


impl Default for Module {
  fn default() -> Self {
    Self {
      compiler_options: Default::default(),
      name: "".to_string(),
      source_file: Arc::new(SourceFile::default()),
      ast: Arc::new(ErlAst::Empty),
      scope: Default::default(),
      errors: RefCell::new(Vec::with_capacity(CompilerOpts::MAX_ERRORS_PER_MODULE * 110 / 100)),
    }
  }
}

impl Debug for Module {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "ErlModule({})", self.name)
  }
}

impl Module {
  /// Create a new empty module
  pub fn new(opt: Arc<CompilerOpts>, source_file: Arc<SourceFile>) -> Self {
    Self {
      compiler_options: opt,
      source_file,
      ..Default::default()
    }
  }

  /// Generic parse helper for any Nom entry point
  fn parse_helper<'a, T>(filename: &Path, input: &'a str, parse_fn: T) -> ErlResult<Self>
    where T: Fn(&'a str) -> nom::IResult<&'a str, Arc<ErlAst>, ErlParserError>
  {
    let mut module = Module::default();

    match parse_fn(input).finish() {
      Ok((tail, forms)) => {
        println!("Parser input «{}»\nParse result AST: «{}»", input, &forms);

        assert!(tail.trim().is_empty(),
                "Not all input was consumed by parse.\n\tTail: «{}»\n\tForms: {:?}", tail, forms);
        module.source_file = SourceFile::new(filename, String::from(input));
        module.ast = forms;

        // Scan AST and find FnDef nodes, update functions knowledge
        Scope::update_from_ast(&module.scope, &module.ast);

        Ok(module)
      }
      Err(err) => Err(ErlError::from_nom_error(input, err)),
    }
  }

  /// Parses code fragment starting with "-module(...)." and containing some function definitions
  /// and the usual module stuff.
  pub fn from_module_source(filename: &Path, input: &str) -> ErlResult<Self> {
    Self::parse_helper(filename, input, ErlParser::parse_module)
  }

  /// Creates a module, where its AST comes from an expression
  pub fn from_expr_source(filename: &Path, input: &str) -> ErlResult<Self> {
    Self::parse_helper(filename, input, ErlParser::parse_expr)
  }

  /// Creates a module, where its AST comes from a function
  pub fn from_fun_source(filename: &Path, input: &str) -> ErlResult<Self> {
    Self::parse_helper(filename, input, ErlParser::parse_fndef)
  }

  /// Creates a 'module', where its AST comes from a typespec source `-spec myfun(...) -> ...`
  pub fn from_fun_spec_source(filename: &Path, input: &str) -> ErlResult<Self> {
    Self::parse_helper(filename, input, ErlParser::parse_fn_spec)
  }

  /// Creates a 'module', where its AST comes from a type `integer() | 42`
  pub fn from_type_source(filename: &Path, input: &str) -> ErlResult<Self> {
    Self::parse_helper(filename, input, ErlParser::parse_type_node)
  }

  /// Adds an error to vector of errors. Returns false when error list is full and the calling code
  /// should attempt to stop.
  pub fn add_error(&self, err: ErlError) -> bool {
    self.errors.borrow_mut().push(err);
    self.errors.borrow().len() < self.compiler_options.max_errors_per_module
  }
}
