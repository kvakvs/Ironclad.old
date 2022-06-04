//! Declares AST node for a clause in `case of` expression
use crate::erl_syntax::erl_ast::ast_iter::TAstNode;
use crate::erl_syntax::erl_ast::AstNode;

/// AST node for a clause in a `case X of` expression.
#[derive(Debug)]
pub struct ErlCaseClause {
  /// A match expression, matched vs. case arg
  pub pattern: AstNode,
  /// Must resolve to bool, or an exception
  pub guard: Option<AstNode>,
  /// Case clause body expression
  pub body: AstNode,
}

impl ErlCaseClause {
  /// Create a new case clause branch
  pub fn new(pattern: AstNode, guard: Option<AstNode>, body: AstNode) -> Self {
    Self { pattern, guard, body }
  }
}

impl std::fmt::Display for ErlCaseClause {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match &self.guard {
      Some(g) => write!(f, "{} when {} -> {};", self.pattern, g, self.body),
      None => write!(f, "{} -> {};", self.pattern, self.body),
    }
  }
}

impl TAstNode for ErlCaseClause {
  fn children(&self) -> Option<Vec<AstNode>> {
    let mut r = self.pattern.children().unwrap_or_default();
    if let Some(g) = &self.guard {
      if let Some(g_children) = g.children() {
        r.extend(g_children.iter().cloned());
      }
    }
    if r.is_empty() {
      None
    } else {
      Some(r)
    }
  }
}
