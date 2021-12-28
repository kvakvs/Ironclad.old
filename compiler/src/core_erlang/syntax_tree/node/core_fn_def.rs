//! Defines a new function in Core Erlang
#![cfg(coreast)]
use std::sync::{Arc, RwLock};

use crate::mfarity::MFArity;
use crate::core_erlang::syntax_tree::node::core_fn_clause::CoreFnClause;
use crate::erl_error::ErlResult;
use crate::source_loc::SourceLoc;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_clause_type::FnClauseType;
use crate::typing::fn_type::FnType;
use crate::typing::scope::Scope;

/// Defines a new function in Core Erlang
/// Argument handling is moved from the clauses into the function body
#[derive(Debug)]
#[cfg(coreast)]
pub struct FnDef {
  /// Source file pointer
  pub location: SourceLoc,
  /// Function name/arity, module is always None
  pub funarity: MFArity,
  // /// Function body AST, for multi-clause functions begins with a Case node.
  // pub body: Arc<CoreAst>,
  // /// Function arguments, CoreAst::Vars with unique numbers
  // pub args: Vec<Arc<CoreAst>>,
  /// Core code split into function clause branches. In Core Erlang these should join as a big
  /// case switch.
  pub clauses: Vec<CoreFnClause>,
}

#[cfg(coreast)]
impl FnDef {
  /// Create a new function definition for Core Erlang AST
  pub fn new(loc: SourceLoc, funarity: MFArity, clauses: Vec<CoreFnClause>) -> Self {
    // assert!(args.iter().all(|el| if let CoreAst::Var(_) = el.deref() { true } else { false }));
    Self {
      location: loc,
      funarity,
      clauses,
    }
  }
}
