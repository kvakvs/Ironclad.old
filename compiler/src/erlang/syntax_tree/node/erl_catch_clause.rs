//! Catch clauses for try-catch block

use std::fmt::Formatter;
use std::sync::Arc;
use crate::erlang::syntax_tree::erl_ast::ErlAst;

/// Catch clause for a try-catch block
#[derive(Debug)]
pub struct CatchClause {
  /// Class in `catch Class:Exc:Stack -> ...`
  pub class_pattern: Arc<ErlAst>,
  /// Exc in `catch Class:Exc:Stack -> ...`
  pub exc_pattern: Arc<ErlAst>,
  /// Stack in `catch Class:Exc:Stack -> ...`
  pub stack_pattern: Option<Arc<ErlAst>>,
  /// When guard: `when X == 0...`
  pub when_guard: Option<Arc<ErlAst>>,
  /// Body of the catch clause, actions if matches
  pub body: Arc<ErlAst>,
}

impl CatchClause {
  /// Create a new catch clause
  pub fn new(class_pattern: Arc<ErlAst>, exc_pattern: Arc<ErlAst>, stack_pattern: Option<Arc<ErlAst>>,
             when_guard: Option<Arc<ErlAst>>, body: Arc<ErlAst>) -> Self {
    Self {
      class_pattern,
      exc_pattern,
      stack_pattern,
      when_guard,
      body,
    }
  }
}

impl std::fmt::Display for CatchClause {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    if let Some(stk) = &self.stack_pattern {
      write!(f, "catch {}:{}:{}", self.class_pattern, self.exc_pattern, stk)?;
    } else {
      write!(f, "catch {}:{}", self.class_pattern, self.exc_pattern)?;
    }
    if let Some(wheng) = &self.when_guard {
      write!(f, "when {}", wheng)?;
    }
    write!(f, " -> {}", self.body)
  }
}
