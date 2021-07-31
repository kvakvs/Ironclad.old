//! Defines an Erlang module ready to be compiled

use ::function_name::named;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::{Arc, Weak, RwLock};
use pest::Parser;

use crate::funarity::FunArity;
use crate::project::compiler_opts::CompilerOpts;
use crate::syntaxtree::erl::erl_ast::{ErlAst};
use crate::typing::unifier::Unifier;
use crate::erl_error::{ErlError, ErlResult};
use crate::project::source_file::SourceFile;
use crate::syntaxtree::erl::erl_parser;
use crate::syntaxtree::erl::node::literal_node::LiteralNode;
use std::collections::hash_map::Entry;
use crate::syntaxtree::erl::node::new_function_node::NewFunctionNode;
use std::cell::{RefCell};

/// Erlang Module consists of
/// - List of forms: attributes, and Erlang functions
/// - Compiler options used to produce this module
pub struct ErlModule {
  /// Options used to build this module. Possibly just a ref to the main project's options
  pub compiler_options: Arc<CompilerOpts>,
  /// Module name atom, as a string
  pub name_atom: String,
  /// The file we're processing AND the file contents (owned by SourceFile)
  pub source_file: Arc<SourceFile>,
  /// AST tree of the module
  pub ast: Rc<RwLock<ErlAst>>,
  /// Type inference and typechecking engine, builds on the parsed AST
  pub unifier: Unifier,
  /// Function definitions of the module
  pub fun_table: HashMap<FunArity, Rc<RefCell<NewFunctionNode>>>,

  /// Weak self-pointer to pass down the chain, similar to C++ smart_ptr_from_this
  pub weak_self: Weak<RwLock<ErlModule>>,

  /// Accumulates found errors in this module. Tries to hard break the operations when error limit
  /// is reached.
  pub errors: Vec<ErlError>,
}


impl Default for ErlModule {
  fn default() -> Self {
    Self {
      compiler_options: Default::default(),
      name_atom: "".to_string(),
      source_file: Arc::new(SourceFile::default()),
      ast: Rc::new(RwLock::new(ErlAst::Empty)),
      unifier: Unifier::default(),
      fun_table: Default::default(),
      weak_self: Weak::new(), // null here
      errors: vec![],
    }
  }
}

impl Debug for ErlModule {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "ErlModule({})", self.name_atom)
  }
}

impl ErlModule {
  /// Create a new empty module
  pub fn new(opt: Arc<CompilerOpts>, source_file: Arc<SourceFile>) -> Self {
    Self {
      compiler_options: opt,
      name_atom: String::new(), // filled during parse
      source_file,
      ast: Rc::new(RwLock::new(ErlAst::Empty)), // filled after parse
      fun_table: Default::default(), // filled during parse
      unifier: Unifier::default(), // filled after parse
      weak_self: Weak::new(), // null here
      errors: Vec::with_capacity(CompilerOpts::MAX_ERRORS_PER_MODULE * 110 / 100),
    }
  }

  /// Create a new empty module without compiler options and a hardcoded testing name
  pub fn new_testing() -> Self {
    Self {
      compiler_options: Arc::new(Default::default()),
      name_atom: String::new(), // filled during parse
      source_file: Arc::new(SourceFile::default()),
      ast: Rc::new(RwLock::new(ErlAst::Empty)), // filled after parse
      unifier: Unifier::default(), // filled after parse
      fun_table: Default::default(), // filled during parse
      weak_self: Weak::new(), // null here
      errors: Vec::with_capacity(CompilerOpts::MAX_ERRORS_PER_MODULE * 110 / 100),
    }
  }

  /// Adds an error to vector of errors. Returns false when error list is full and the calling code
  /// should attempt to stop.
  pub fn add_error(&mut self, err: ErlError) -> bool {
    self.errors.push(err);
    self.errors.len() < self.compiler_options.max_errors_per_module
  }

  /// Parse self.source_file
  pub fn parse_and_unify(&mut self) -> ErlResult<()> {
    let sf = self.source_file.clone(); // lure the borrow checker to letting us use text
    self.parse_and_unify_str(erl_parser::Rule::module, &sf.text)
  }

  /// Create a dummy sourcefile and parse it starting with the given parser rule.
  /// This updates the self.fun_table and self.ast
  pub fn parse_and_unify_str(&mut self,
                             rule: erl_parser::Rule, input: &str) -> ErlResult<()> {
    let parse_output = match erl_parser::ErlParser::parse(rule, input) {
      Ok(mut root) => root.next().unwrap(),
      Err(bad) => {
        assert!(false, "Parse failed {}", bad);
        return Err(ErlError::from(bad));
      }
    };

    self.source_file = SourceFile::new(&PathBuf::from("<test>"), String::from(""));
    self.ast = {
      // Parse tree to raw AST
      let mut intermediate_ast = self.to_ast_single_node(parse_output)?;

      // Process raw AST to a cleaned AST with some fields edited  and some nodes replaced
      self.postprocess_ast(&mut intermediate_ast)?;

      Rc::new(RwLock::new(intermediate_ast))
    };

    self.unifier = Unifier::new(self).unwrap();
    Ok(())
  }

  /// Create a dummy sourcefile and parse ANY given parser rule, do not call the unifier.
  /// This updates only self.ast
  #[named]
  pub fn parse_str(&mut self, rule: erl_parser::Rule, input: &str) -> ErlResult<()> {
    let parse_output = match erl_parser::ErlParser::parse(rule, input) {
      Ok(mut root) => root.next().unwrap(),
      Err(bad) => {
        assert!(false, "{}, failed {}", function_name!(), bad);
        return Err(ErlError::from(bad));
      }
    };

    // Create a fake source file with no filename and copy of input (for error reporting)
    self.source_file = SourceFile::new(&PathBuf::from("<test>"), String::from(input));

    // build initial AST from parse
    let mut ast0 = self.to_ast_single_node(parse_output)?;

    // Modify some AST nodes as required
    self.postprocess_ast(&mut ast0)?;

    self.ast = Rc::new(RwLock::new(ast0));
    Ok(())
  }

  /// For an expression check whether it is a constant expression, and whether it points to some
  /// known function in this module. Arity is provided as the expression might be just an atom.
  pub fn find_function_expr_arity(&mut self, expr: &ErlAst, arity: usize) -> Option<Rc<RefCell<NewFunctionNode>>> {
    if let ErlAst::Lit(_loc, lit) = expr {
      // A single atom points to a possible existing function of `arity` in the current module
      if let LiteralNode::Atom(a) = lit {
        let fa = FunArity { name: a.clone(), arity };

        match self.fun_table.entry(fa) {
          Entry::Occupied(e) => return Some(e.get().clone()), // found!
          Entry::Vacant(_) => return None, // not found
        }
      }
    }
    None
  }
}
