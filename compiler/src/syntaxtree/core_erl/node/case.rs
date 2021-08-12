//! Defines a case switch in Core Erlang

use crate::syntaxtree::core_erl::core_ast::CoreAst;

/// Case clause checks the input expression against `match_expr` and if it matches and if the guard
/// is true, the body will be executed.
pub struct CaseClause {
  pub match_expr: Box<CoreAst>,
  pub guard_cond: Option<CoreAst>,
  pub body: Box<CoreAst>,
}

/// Case replaces Erlang constructs such as multiple function clauses (merged into one with a case),
/// `if` operator, `try of`, and `case of`.
pub struct Case {
  pub expr: Box<CoreAst>,
  pub clauses: Vec<CaseClause>,
}
