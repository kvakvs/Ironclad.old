//! Function type, containing clauses

use std::sync::Arc;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_clause_type::FnClauseType;

/// Function type
#[derive(Debug, Eq, PartialEq)]
pub struct FnType {
  /// For convenience arity is stored here, but each clause has same arity too
  arity: usize,
  /// Function clauses
  clauses: Vec<Arc<FnClauseType>>,
}

impl FnType {
  /// Create a new function type with clauses
  pub fn new(arity: usize, clauses: &[Arc<FnClauseType>]) -> Self {
    if cfg!(debug_assertions) {
      let in_arities: Vec<String> = clauses.iter()
          .map(|fc| format!("{}", fc.arity()))
          .collect();
      let arities_str = in_arities.join(", ");

      assert!(clauses.iter().all(|fc| fc.arity() == arity),
              "All clauses must have arity {}: found {}", arity, arities_str);
    }

    Self {
      arity,
      clauses: clauses.into(),
    }
  }

  /// Retrieve arity
  pub fn arity(&self) -> usize { self.arity }

  /// Read clauses vector
  pub fn clauses(&self) -> &Vec<Arc<FnClauseType>> { &self.clauses }

  /// Read one clause
  pub fn clause(&self, index: usize) -> &Arc<FnClauseType> { &self.clauses[index] }

  /// Check whether argument list can be passed to any of the clauses
  pub fn get_compatible_clauses(&self, args: &[Arc<ErlType>]) -> Vec<Arc<FnClauseType>> {
    self.clauses.iter()
        .filter(|fc| fc.can_accept_args(args))
        .cloned()
        .collect()
  }
}
