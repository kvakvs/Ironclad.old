//! Defines a case switch in Core Erlang

use crate::syntaxtree::core_erl::core_ast::CoreAst;
use crate::typing::typevar::TypeVar;

/// Case clause checks the input expression against `match_expr` and if it matches and if the guard
/// is true, the body will be executed.
pub struct CaseClause {
  /// The match expression
  pub match_expr: Box<CoreAst>,
  /// Guard condition, None if there's no condition (always true)
  pub guard_cond: Option<CoreAst>,
  /// Clause body
  pub body: Box<CoreAst>,
  /// Case clause type
  pub ret_ty: TypeVar,
}

/// Case replaces Erlang constructs such as multiple function clauses (merged into one with a case),
/// `if` operator, `try of`, and `case of`.
pub struct Case {
  /// Case switch expression
  pub expr: Box<CoreAst>,
  /// Case clauses in order
  pub clauses: Vec<CaseClause>,
}
