//! Defines an Erlang module ready to be compiled
use crate::erl_syntax::erl_ast::AstNode;
use nom::Finish;
use std::cell::RefCell;
use std::fmt;
use std::fmt::Debug;
use std::path::Path;
use std::sync::{Arc, RwLock};

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_error::ErlError;
use crate::erl_syntax::parsers::defs::{ParserInput, ParserResult};
use crate::erl_syntax::parsers::misc::panicking_parser_error_reporter;
use crate::erl_syntax::parsers::parse_expr::parse_expr;
use crate::erl_syntax::parsers::parse_fn::parse_fndef;
use crate::erl_syntax::parsers::parse_module;
use crate::erl_syntax::parsers::parse_type::ErlTypeParser;
use crate::error::ic_error::IcResult;
use crate::project::compiler_opts::CompilerOpts;
use crate::source_file::{SourceFile, SourceFileImpl};
use crate::typing::scope::Scope;

/// Erlang Module consists of
/// - List of forms: attributes, and Erlang functions
/// - Compiler options used to produce this module
pub struct ErlModule {
  /// Options used to build this module. Possibly just a ref to the main project's options
  pub compiler_options: Arc<CompilerOpts>,
  /// Module name atom, as a string
  pub name: String,
  /// The file we're processing AND the file contents (owned by SourceFile)
  pub source_file: SourceFile,

  /// AST tree of the module.
  pub ast: AstNode,

  // /// Collection of module functions and a lookup table
  // pub registry: RwLock<FuncRegistry>,
  /// Module level scope, containing functions
  pub scope: Arc<RwLock<Scope>>,

  /// Accumulates found errors in this module. Tries to hard break the operations when error limit
  /// is reached.
  pub errors: RefCell<Vec<ErlError>>,
}

impl Default for ErlModule {
  fn default() -> Self {
    Self {
      compiler_options: Default::default(),
      name: "".to_string(),
      source_file: Arc::new(SourceFileImpl::default()),
      ast: AstNodeImpl::new_empty(),
      scope: Default::default(),
      errors: RefCell::new(Vec::with_capacity(CompilerOpts::MAX_ERRORS_PER_MODULE * 110 / 100)),
    }
  }
}

impl Debug for ErlModule {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "ErlModule({})", self.name)
  }
}

impl ErlModule {
  /// Create a new empty module
  pub fn new(opt: Arc<CompilerOpts>, source_file: SourceFile) -> Self {
    Self {
      compiler_options: opt,
      source_file,
      ..Default::default()
    }
  }

  /// Generic parse helper for any Nom entry point
  pub fn parse_helper<'a, T>(filename: &Path, input_s: &str, parse_fn: T) -> IcResult<Self>
  where
    T: Fn(ParserInput<'a>) -> ParserResult<AstNode>,
  {
    // println!("Parsing from {}", filename.to_string_lossy());
    // println!("Input=«{}»", input_s);

    let input = ParserInput::new_str(input_s);
    let mut module = ErlModule::default();
    let (tail, forms) =
      panicking_parser_error_reporter(input.clone(), parse_fn(input.clone()).finish());

    // println!("Parse result AST: «{}»", &forms);

    assert!(
      tail.trim().is_empty(),
      "Not all input was consumed by parse.\n\tTail: «{}»\n\tForms: {}",
      tail,
      forms
    );
    // TODO: This assignment below should be happening earlier before parse, as parse can refer to the SourceFile
    module.source_file = SourceFileImpl::new(filename, input.to_string());
    module.ast = forms;

    // Scan AST and find FnDef nodes, update functions knowledge
    Scope::update_from_ast(&module.scope, &module.ast);

    Ok(module)
  }

  /// Parses code fragment starting with "-module(...)." and containing some function definitions
  /// and the usual module stuff.
  pub fn from_module_source(filename: &Path, input: &str) -> IcResult<Self> {
    Self::parse_helper(filename, input, parse_module)
  }

  /// Creates a module, where its AST comes from an expression
  pub fn from_expr_source(filename: &Path, input: &str) -> IcResult<Self> {
    Self::parse_helper(filename, input, parse_expr)
  }

  /// Creates a module, where its AST comes from a function
  pub fn from_fun_source(filename: &Path, input: &str) -> IcResult<Self> {
    Self::parse_helper(filename, input, parse_fndef)
  }

  /// Creates a 'module', where its AST comes from a typespec source `-spec myfun(...) -> ...`
  pub fn from_fun_spec_source(filename: &Path, input: &str) -> IcResult<Self> {
    Self::parse_helper(filename, input, ErlTypeParser::fn_spec_attr)
  }

  /// Creates a 'module', where its AST comes from a type `integer() | 42`
  pub fn from_type_source(filename: &Path, input: &str) -> IcResult<Self> {
    Self::parse_helper(filename, input, ErlTypeParser::parse_type_node)
  }

  /// Adds an error to vector of errors. Returns false when error list is full and the calling code
  /// should attempt to stop.
  pub fn add_error(&self, err: ErlError) -> bool {
    self.errors.borrow_mut().push(err);
    self.errors.borrow().len() < self.compiler_options.max_errors_per_module
  }

  // /// Updates `self.ast` with preprocessor nodes interpreted, and removed. All `?MACRO` values are
  // /// interpreted and replaced with values.
  // pub fn interpret_preprocessor_nodes(&mut self) -> IcResult<()> {
  //   // let (mod_name, forms) = self.ast.as_module();
  //   // let mut processed_forms = Vec::<AstNode>::default();
  //   // for f in forms {
  //   //   // Preprocessor nodes can only occur on top level of a module,
  //   //   // but nodes which need substitutions of `?MACRO`s can occur anywhere.
  //   //   interpret_preprocessor_node(self, f)?;
  //   // }
  //   // self.ast = processed_ast;
  //   Ok(())
  // }
}
