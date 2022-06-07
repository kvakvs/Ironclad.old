//! Catch clauses for try-catch block

use crate::erl_syntax::erl_ast::ast_iter::AstParentNodeT;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_exception_pattern::ExceptionPattern;
use std::fmt::Formatter;

/// Catch clause for a try-catch block
#[derive(Debug)]
pub struct CatchClause {
  /// Exception pattern in `catch Class:Exc:Stack -> ...`
  pub exc_pattern: ExceptionPattern,
  /// When guard: `when X == 0...`
  pub when_guard: Option<AstNode>,
  /// Body of the catch clause, actions if matches
  pub body: AstNode,
}

impl CatchClause {
  /// Create a new catch clause
  pub fn new(exc_pattern: ExceptionPattern, when_guard: Option<AstNode>, body: AstNode) -> Self {
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

impl AstParentNodeT for CatchClause {
  fn children(&self) -> Option<Vec<AstNode>> {
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
