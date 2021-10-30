//! Defines a new function in Core Erlang
use std::ops::Deref;
use std::sync::Arc;

use crate::mfarity::MFArity;
use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::source_loc::SourceLoc;

/// Defines a new function in Core Erlang
/// Argument handling is moved from the clauses into the function body
#[derive(Debug)]
pub struct FnDef {
  /// Source file pointer
  pub location: SourceLoc,
  /// Function name/arity, module is always None
  pub funarity: MFArity,
  /// Function body AST, for multi-clause functions begins with a Case node.
  pub body: Arc<CoreAst>,
  /// Function arguments, CoreAst::Vars with unique numbers
  pub args: Vec<Arc<CoreAst>>,
}

impl FnDef {
  /// Create a new function definition for Core Erlang AST
  pub fn new(loc: SourceLoc, funarity: MFArity, body: Arc<CoreAst>, args: Vec<Arc<CoreAst>>) -> Self {
    assert!(args.iter().all(|el| if let CoreAst::Var(_) = el.deref() { true } else { false }));
    Self {
      location: loc,
      funarity,
      body,
      args,
    }
  }
}
