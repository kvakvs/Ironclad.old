//! Declares AST node for a clause in `case of` expression
use crate::syntaxtree::erl::erl_ast::ErlAst;
use std::fmt::Formatter;
use crate::typing::typevar::TypeVar;

/// AST node for a clause in a `case X of` expression.
// #[derive(PartialEq)]
pub struct CaseClause {
  /// A match expression, matched vs. case arg
  pub cond: Box<ErlAst>,
  /// Must resolve to bool, or an exception
  pub guard: Box<ErlAst>,
  /// Case clause body expression
  pub body: Box<ErlAst>,
  /// Clause body type, for type inference
  pub ty: TypeVar,
}

impl std::fmt::Display for CaseClause {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{} when {} -> {};", self.cond, self.guard, self.body)
  }
}