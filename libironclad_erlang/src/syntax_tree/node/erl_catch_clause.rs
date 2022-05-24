//! Catch clauses for try-catch block

use crate::syntax_tree::erl_ast::ast_iter::AstNode;
use crate::syntax_tree::erl_ast::ErlAst;
use crate::syntax_tree::node::erl_exception_pattern::ExceptionPattern;
use std::fmt::Formatter;
use std::sync::Arc;

/// Catch clause for a try-catch block
#[derive(Debug)]
pub struct CatchClause {
  /// Exception pattern in `catch Class:Exc:Stack -> ...`
  pub exc_pattern: ExceptionPattern,
  /// When guard: `when X == 0...`
  pub when_guard: Option<Arc<ErlAst>>,
  /// Body of the catch clause, actions if matches
  pub body: Arc<ErlAst>,
}

impl CatchClause {
  /// Create a new catch clause
  pub fn new(
    exc_pattern: ExceptionPattern,
    when_guard: Option<Arc<ErlAst>>,
    body: Arc<ErlAst>,
  ) -> Self {
    Self { exc_pattern, when_guard, body }
  }
}

impl std::fmt::Display for CatchClause {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    if let Some(stk) = &self.exc_pattern.stack {
      write!(f, "catch {}:{}:{}", self.exc_pattern.class, self.exc_pattern.error, stk)?;
    } else {
      write!(f, "catch {}:{}", self.exc_pattern.class, self.exc_pattern.error)?;
    }
    if let Some(wheng) = &self.when_guard {
      write!(f, "when {}", wheng)?;
    }
    write!(f, " -> {}", self.body)
  }
}

impl AstNode for CatchClause {
  fn children(&self) -> Option<Vec<Arc<ErlAst>>> {
    let mut r = Vec::default();
    if let Some(c) = self.exc_pattern.children() {
      r.extend(c.iter().cloned());
    }
    if let Some(wheng) = &self.when_guard {
      if let Some(wheng_children) = wheng.children() {
        r.extend(wheng_children.iter().cloned());
      }
    }
    if let Some(b_children) = self.body.children() {
      r.extend(b_children.iter().cloned());
    }
    if r.is_empty() {
      None
    } else {
      Some(r)
    }
  }
}
