//! Exception pattern for `try-catch Class:Exception:Stack -> ...`

use crate::erl_syntax::erl_ast::ast_iter::IterableAstNodeT;
use crate::erl_syntax::erl_ast::AstNode;

/// Represents an exception pattern in catch clause for `try-catch Class:Exception:Stack -> ...`
#[derive(Debug)]
pub struct ExceptionPattern {
  /// The `Class:_Err:_Stk` part of the pattern
  pub class: AstNode,
  /// The `_Cls:Error:_Stk...` part of the pattern
  pub error: AstNode,
  /// The `_Cls:_Err:StackTrace...` part of the pattern if the language version supports it
  pub stack: Option<AstNode>,
}

impl ExceptionPattern {
  /// Creates a new `ExceptionPattern`
  pub(crate) fn new(
    class_pattern: AstNode,
    err_pattern: AstNode,
    stack_pattern: Option<AstNode>,
  ) -> Self {
    Self {
      class: class_pattern,
      error: err_pattern,
      stack: stack_pattern,
    }
  }
}

impl IterableAstNodeT for ExceptionPattern {
  fn children(&self) -> Option<Vec<AstNode>> {
    let mut r = Vec::default();
    if let Some(c) = self.class.children() {
      r.extend(c.iter().cloned());
    }
    if let Some(c) = self.error.children() {
      r.extend(c.iter().cloned());
    }
    if let Some(stack) = &self.stack {
      if let Some(c) = stack.children() {
        r.extend(c.iter().cloned());
      }
    }
    if r.is_empty() {
      None
    } else {
      Some(r)
    }
  }
}
