//! Declares AST node for a clause in `case of` expression
use std::fmt::Formatter;
use std::sync::Arc;

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::typing::typevar::TypeVar;

/// AST node for a clause in a `case X of` expression.
// #[derive(PartialEq)]
pub struct ErlCaseClause {
  /// A match expression, matched vs. case arg
  pub cond: Arc<ErlAst>,
  /// Must resolve to bool, or an exception
  pub guard: Arc<ErlAst>,
  /// Case clause body expression
  pub body: Arc<ErlAst>,
  /// Clause body type, for type inference
  pub ty: TypeVar,
}

impl std::fmt::Display for ErlCaseClause {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{} when {} -> {};", self.cond, self.guard, self.body)
  }
}