//! Exception pattern for `try-catch Class:Exception:Stack -> ...`

use std::sync::Arc;
use crate::erlang::syntax_tree::erl_ast::ErlAst;

/// Represents an exception pattern in catch clause for `try-catch Class:Exception:Stack -> ...`
#[derive(Debug)]
pub struct ExceptionPattern {
  /// The `Class:_Err:_Stk` part of the pattern
  pub class: Arc<ErlAst>,
  /// The `_Cls:Error:_Stk...` part of the pattern
  pub error: Arc<ErlAst>,
  /// The `_Cls:_Err:StackTrace...` part of the pattern if the language version supports it
  pub stack: Option<Arc<ErlAst>>,
}

impl ExceptionPattern {
  /// Creates a new `ExceptionPattern`
  pub fn new(class_pattern: Arc<ErlAst>,
             err_pattern: Arc<ErlAst>,
             stack_pattern: Option<Arc<ErlAst>>) -> Self {
    Self {
      class: class_pattern,
      error: err_pattern,
      stack: stack_pattern,
    }
  }
}
