//! Defines a new function in Core Erlang
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

  /// Produce a function `ErlType` with all clauses and their return types
  pub fn synthesize_function_type(&self, _scope: &Arc<RwLock<Scope>>) -> ErlResult<Arc<ErlType>> {
    let clauses_r: ErlResult<Vec<Arc<FnClauseType>>> = self.clauses.iter()
        .map(|fnc| fnc.synthesize_clause_type(&fnc.scope))
        .collect();
    let clauses = clauses_r?;

    println!("synth_function_type for {}: {:?}", self.funarity, clauses);

    let fn_type = FnType::new(self.funarity.arity, &clauses);
    let synthesized_t = ErlType::Fn(fn_type.into()).into();
    Ok(synthesized_t)
  }

  /// Produce a function return type, as union of all clauses returns
  pub fn synthesize_return_type(&self, _scope: &RwLock<Scope>) -> ErlResult<Arc<ErlType>> {
    // TODO: Filter out incompatible clauses
    let clauses_ret: ErlResult<Vec<Arc<ErlType>>> = self.clauses.iter()
        .map(|fnc| fnc.synthesize_clause_return_type(&fnc.scope))
        .collect();
    let synthesized_t = ErlType::new_union(&clauses_ret?);
    Ok(synthesized_t)
  }
}
