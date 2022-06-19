//! Defines an Erlang module ready to be compiled
use crate::erl_syntax::erl_ast::AstNode;
use nom::Finish;
use std::cell::RefCell;
use std::fmt;
use std::fmt::Debug;
use std::sync::{Arc, RwLock};

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_error::ErlError;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{
  panicking_parser_error_reporter, panicking_tokenizer_error_reporter,
};
use crate::erl_syntax::parsers::parse_expr::parse_expr;
use crate::erl_syntax::parsers::parse_fn::parse_fndef;
use crate::erl_syntax::parsers::parse_module;
use crate::erl_syntax::parsers::parse_type::ErlTypeParser;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::parsers::parser_scope::{ParserScope, ParserScopeImpl};
use crate::erl_syntax::token_stream::tok_input::{TokenizerInput, TokensResult};
use crate::erl_syntax::token_stream::token::Token;
use crate::erl_syntax::token_stream::tokenizer::tokenize_source;
use crate::error::ic_error::IcResult;
use crate::project::compiler_opts::{CompilerOpts, CompilerOptsImpl};
use crate::project::ErlProject;
use crate::source_file::{SourceFile, SourceFileImpl};
use crate::typing::scope::Scope;

/// Erlang Module consists of
/// - List of forms: attributes, and Erlang functions
/// - Compiler options used to produce this module
pub struct ErlModuleImpl {
  /// Options used to build this module. Possibly just a ref to the main project's options
  pub compiler_options: CompilerOpts,
  /// Module name atom, as a string
  pub name: String,
  /// The file we're processing AND the file contents (owned by SourceFile)
  pub source_file: SourceFile,
  /// Stores state of preprocessor defines during parse, and then final state after parse (useful for testing)
  pub parser_scope: ParserScope,
  /// AST tree of the module.
  pub ast: AstNode,
  /// Module level scope, containing functions
  pub scope: Arc<RwLock<Scope>>,
  /// Accumulates found errors in this module. Tries to hard break the operations when error limit
  /// is reached.
  pub errors: RefCell<Vec<ErlError>>,
}

/// Wraps module into runtime-lockable refcount
pub type ErlModule = Arc<RwLock<ErlModuleImpl>>;

impl Default for ErlModuleImpl {
  fn default() -> Self {
    Self {
      compiler_options: Default::default(),
      name: "".to_string(),
      source_file: Arc::new(SourceFileImpl::default()),
      parser_scope: ParserScopeImpl::new_empty().into(),
      ast: AstNodeImpl::new_empty("dummy node for module root".to_string()),
      scope: Default::default(),
      errors: RefCell::new(Vec::with_capacity(CompilerOptsImpl::MAX_ERRORS_PER_MODULE * 110 / 100)),
    }
  }
}

impl Debug for ErlModuleImpl {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "ErlModule({})", self.name)
  }
}

impl ErlModuleImpl {
  /// Create a new empty module
  pub fn new(opt: CompilerOpts, source_file: SourceFile) -> ErlModule {
    RwLock::new(Self {
      compiler_options: opt,
      source_file,
      ..Default::default()
    })
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

  /// Filter through the tokens array and produce a new token array with preprocessor directives
  /// eliminated, files included and macros substituted.
  pub fn preprocess(_module: &ErlModule, tokens: &[Token]) -> IcResult<Vec<Token>> {
    Ok(
      tokens
        .iter()
        .filter(|t| !t.is_newline()) // throw away newlines
        .cloned()
        .collect(),
    )
  }

  /// Generic parse helper for any Nom entry point.
  /// Input comes as string in the `SourceFile`, the input is tokenized and then parsed.
  pub fn parse_helper<T>(
    project: &ErlProject,
    src_file: SourceFile,
    parse_fn: T,
    compiler_options: Option<CompilerOpts>,
  ) -> IcResult<ErlModule>
  where
    T: Fn(ParserInput) -> ParserResult<AstNode>,
  {
    let mut module_impl = ErlModuleImpl {
      source_file: src_file.clone(),
      ..ErlModuleImpl::default()
    };
    if let Some(o) = compiler_options {
      module_impl.compiler_options = o;
    }
    let module: ErlModule = RwLock::new(module_impl).into();

    let tok_stream1 = ErlModuleImpl::tokenize_helper(project, src_file.clone(), tokenize_source)?;
    let tok_stream2 = ErlModuleImpl::preprocess(&module, &tok_stream1)?;

    let (tail, forms) = {
      let tokens_input = ParserInput::new(&src_file, &tok_stream2);
      panicking_parser_error_reporter(
        src_file.text.as_str(),
        tokens_input.clone(),
        parse_fn(tokens_input.clone()).finish(),
      )
    };

    assert!(
      tail.is_empty(),
      "Not all input was consumed by parse.\n\tTail: «{:?}»\n\tForms: {}",
      tail,
      forms
    );

    // TODO: This assignment below should be happening earlier before parse, as parse can refer to the SourceFile
    if let Ok(mut w_module) = module.write() {
      w_module.ast = forms;
    } else {
      panic!("Can't lock module for updating AST field")
    }

    // Scan AST and find FnDef nodes, update functions knowledge
    // Scope::update_from_ast(&module.scope, &module.ast);

    Ok(module)
  }

  /// Parses code fragment starting with "-module(...)." and containing some function definitions
  /// and the usual module stuff.
  pub fn from_module_source(
    project: &ErlProject,
    source_file: &SourceFile,
    compiler_options: Option<CompilerOpts>,
  ) -> IcResult<ErlModule> {
    Self::parse_helper(project, source_file.clone(), parse_module, compiler_options)
  }

  /// Creates a module, where its AST comes from an expression
  pub fn from_expr_source(
    project: &ErlProject,
    source_file: &SourceFile,
    compiler_options: Option<CompilerOpts>,
  ) -> IcResult<ErlModule> {
    Self::parse_helper(project, source_file.clone(), parse_expr, compiler_options)
  }

  /// Creates a module, where its AST comes from a function
  pub fn from_fun_source(
    project: &ErlProject,
    source_file: &SourceFile,
    compiler_options: Option<CompilerOpts>,
  ) -> IcResult<ErlModule> {
    Self::parse_helper(project, source_file.clone(), parse_fndef, compiler_options)
  }

  /// Creates a 'module', where its AST comes from a typespec source `-spec myfun(...) -> ...`
  pub fn from_fun_spec_source(
    project: &ErlProject,
    source_file: &SourceFile,
    compiler_options: Option<CompilerOpts>,
  ) -> IcResult<ErlModule> {
    Self::parse_helper(project, source_file.clone(), ErlTypeParser::fn_spec_attr, compiler_options)
  }

  /// Creates a 'module', where its AST comes from a type `integer() | 42`
  pub fn from_type_source(
    project: &ErlProject,
    source_file: &SourceFile,
    compiler_options: Option<CompilerOpts>,
  ) -> IcResult<ErlModule> {
    Self::parse_helper(
      project,
      source_file.clone(),
      ErlTypeParser::parse_type_node,
      compiler_options,
    )
  }

  /// Adds an error to vector of errors. Returns false when error list is full and the calling code
  /// should attempt to stop.
  pub fn add_error(&self, err: ErlError) -> bool {
    self.errors.borrow_mut().push(err);
    self.errors.borrow().len() < self.compiler_options.max_errors_per_module
  }
}

/// Read accessor
pub fn erl_module_ast(m: &ErlModule) -> AstNode {
  if let Ok(read_m) = m.read() {
    read_m.ast.clone()
  } else {
    panic!("Can't lock module to access AST")
  }
}

/// Read accessor
pub fn erl_module_parser_scope(m: &ErlModule) -> ParserScope {
  if let Ok(read_m) = m.read() {
    read_m.parser_scope.clone()
  } else {
    panic!("Can't lock module to access parser scope")
  }
}

/// Read accessor
pub fn erl_module_scope(m: &ErlModule) -> Arc<RwLock<Scope>> {
  if let Ok(read_m) = m.read() {
    read_m.scope.clone()
  } else {
    panic!("Can't lock module to access module scope")
  }
}
