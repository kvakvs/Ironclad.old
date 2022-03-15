//! Declares AST node for a clause in `case of` expression
use std::fmt::Formatter;
use std::sync::Arc;
use crate::erlang::syntax_tree::erl_ast::ast_iter::AstNode;

use crate::erlang::syntax_tree::erl_ast::ErlAst;

/// AST node for a clause in a `case X of` expression.
#[derive(Debug)]
pub struct ErlCaseClause {
  /// A match expression, matched vs. case arg
  pub pattern: Arc<ErlAst>,
  /// Must resolve to bool, or an exception
  pub guard: Option<Arc<ErlAst>>,
  /// Case clause body expression
  pub body: Arc<ErlAst>,
}

impl ErlCaseClause {
  /// Create a new case clause branch
  pub fn new(pattern: Arc<ErlAst>, guard: Option<Arc<ErlAst>>, body: Arc<ErlAst>) -> Self {
    Self { pattern, guard, body }
  }
}

impl std::fmt::Display for ErlCaseClause {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match &self.guard {
      Some(g) => write!(f, "{} when {} -> {};", self.pattern, g, self.body),
      None => write!(f, "{} -> {};", self.pattern, self.body),
    }
  }
}

impl AstNode for ErlCaseClause {
  fn children(&self) -> Option<Vec<Arc<ErlAst>>> {
    let mut r = self.pattern.children().unwrap_or_else(Vec::default);
    if let Some(g) = &self.guard {
      if let Some(g_children) = g.children() {
        r.extend(g_children.iter().cloned());
      }
    }
    Some(r)
  }
}