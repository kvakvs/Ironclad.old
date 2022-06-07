//! A branch of `if COND -> EXPR; ... end`

use crate::erl_syntax::erl_ast::ast_iter::AstParentNodeT;
use crate::erl_syntax::erl_ast::AstNode;

/// AST node for a clause in a `if COND -> EXPR; ... end` statement.
#[derive(Debug)]
pub struct ErlIfClause {
  /// A condition expression
  pub cond: AstNode,
  /// If clause body expression
  pub body: AstNode,
}

impl ErlIfClause {
  /// Create a new `if` clause branch
  pub fn new(cond: AstNode, body: AstNode) -> Self {
    Self { cond, body }
  }
}

impl std::fmt::Display for ErlIfClause {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{} -> {}", self.cond, self.body)
  }
}

impl AstParentNodeT for ErlIfClause {
  fn children(&self) -> Option<Vec<AstNode>> {
    let mut r = self.cond.children().unwrap_or_default();
    if let Some(body_children) = self.body.children() {
      r.extend(body_children.iter().cloned());
    }
    if r.is_empty() {
      None
    } else {
      Some(r)
    }
  }
}
