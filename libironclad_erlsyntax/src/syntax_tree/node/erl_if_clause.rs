//! A branch of `if COND -> EXPR; ... end`

use std::sync::Arc;
use crate::syntax_tree::erl_ast::ast_iter::AstNode;
use crate::syntax_tree::erl_ast::ErlAst;

/// AST node for a clause in a `if COND -> EXPR; ... end` statement.
#[derive(Debug)]
pub struct ErlIfClause {
  /// A condition expression
  pub cond: Arc<ErlAst>,
  /// If clause body expression
  pub body: Arc<ErlAst>,
}

impl ErlIfClause {
  /// Create a new `if` clause branch
  pub fn new(cond: Arc<ErlAst>, body: Arc<ErlAst>) -> Self {
    Self { cond, body }
  }
}

impl std::fmt::Display for ErlIfClause {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{} -> {}", self.cond, self.body)
  }
}

impl AstNode for ErlIfClause {
  fn children(&self) -> Option<Vec<Arc<ErlAst>>> {
    let mut r = self.cond.children().unwrap_or_default();
    if let Some(body_children) = self.body.children() {
      r.extend(body_children.iter().cloned());
    }
    if r.is_empty() { None } else { Some(r) }
  }
}