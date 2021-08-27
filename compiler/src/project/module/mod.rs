//! Defines an Erlang module ready to be compiled

use ::function_name::named;
use pest::Parser;
use std::cell::RefCell;
use std::fmt::Debug;
use std::fmt;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};

use crate::erl_error::{ErlError, ErlResult};
use crate::project::compiler_opts::CompilerOpts;
use crate::project::source_file::SourceFile;
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::erl_parser;
use crate::typing::unifier::Unifier;
use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::core_erlang::syntax_tree::core_ast_builder::CoreAstBuilder;
use crate::project::module::func_registry::FuncRegistry;

pub mod func_registry;

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

  /// Core Erlang AST tree of the module
  pub core_ast: Arc<CoreAst>,

  /// Type inference and typechecking engine, builds on the parsed AST
  pub unifier: Unifier,

  /// Collection of module functions and a lookup table
  pub registry: RwLock<FuncRegistry>,

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
      core_ast: Arc::new(CoreAst::Empty),
      unifier: Default::default(),
      registry: RwLock::new(FuncRegistry::default()),
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

  /// Adds an error to vector of errors. Returns false when error list is full and the calling code
  /// should attempt to stop.
  pub fn add_error(&self, err: ErlError) -> bool {
    self.errors.borrow_mut().push(err);
    self.errors.borrow().len() < self.compiler_options.max_errors_per_module
  }

  /// Parse self.source_file as Erlang syntax
  pub fn parse_and_unify_erlang(&mut self) -> ErlResult<()> {
    let sf = self.source_file.clone(); // lure the borrow checker to letting us use text
    self.parse_and_unify_erl_str(erl_parser::Rule::module, &sf.text)
  }

  /// Create a dummy sourcefile and parse it starting with the given parser rule.
  /// This updates the self.fun_table and self.ast
  #[named]
  pub fn parse_and_unify_erl_str(&mut self,
                                 rule: erl_parser::Rule, input: &str) -> ErlResult<()> {
    let parse_output = match erl_parser::ErlParser::parse(rule, input) {
      Ok(mut root) => root.next().unwrap(),
      Err(bad) => {
        panic!("Parse failed {}", bad);
        // return Err(ErlError::from(bad));
      }
    };

    self.source_file = SourceFile::new(&PathBuf::from("<test>"), String::from(""));

    // Parse tree to raw AST
    self.ast = self.build_ast_single_node(parse_output)?;
    println!("\n{}: {}", function_name!(), self.ast);

    // Rebuild Core AST from Erlang AST
    self.core_ast = CoreAstBuilder::build(self, &self.ast);

    self.unifier = Unifier::new(self).unwrap();
    Ok(())
  }

  /// Create a dummy sourcefile and parse ANY given parser rule, do not call the unifier.
  /// This updates only self.ast
  #[named]
  pub fn parse_erl_str(&mut self, rule: erl_parser::Rule, input: &str) -> ErlResult<()> {
    let parse_output = match erl_parser::ErlParser::parse(rule, input) {
      Ok(mut root) => root.next().unwrap(),
      Err(bad) => {
        panic!("{}, failed {}", function_name!(), bad);
        // return Err(ErlError::from(bad));
      }
    };

    // Create a fake source file with no filename and copy of input (for error reporting)
    self.source_file = SourceFile::new(&PathBuf::from("<test>"), String::from(input));

    // build initial AST from parse
    self.ast = self.build_ast_single_node(parse_output)?;
    println!("\n{}: {}", function_name!(), self.ast);

    self.core_ast = CoreAstBuilder::build(self, &self.ast);

    Ok(())
  }
}
