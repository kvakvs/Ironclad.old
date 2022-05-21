//! Defines primitive operations for BEAM assembly and BEAM VM, which are not part of Core Erlang.
#![cfg(coreast)]

use ::function_name::named;
use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use std::sync::Arc;
use std::fmt::Formatter;

/// Describes an exception kind
#[derive(Debug)]
#[cfg(coreast)]
pub enum ExceptionType {
  /// `erlang:error`
  Error,
  /// `erlang:exit`
  Exit,
  /// `erlang:throw`
  Throw,
}

#[cfg(coreast)]
impl std::fmt::Display for ExceptionType {
  #[named]
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      ExceptionType::Error => write!(f, "error"),
      ExceptionType::Exit => write!(f, "exit"),
      ExceptionType::Throw => write!(f, "throw"),
    }
  }
}

/// Primitive operation, not part of Core Erlang language but is useful to express Erlang constructs
#[cfg(coreast)]
pub enum PrimOp {
  /// Raises an exception of type
  Raise {
    /// Exception kind
    exc: ExceptionType,
    /// The value to be thrown
    expr: Arc<CoreAst>,
  },
  /// primop `exc_trace`
  ExcTrace,
}

#[cfg(coreast)]
impl std::fmt::Debug for PrimOp {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self)
  }
}

#[cfg(coreast)]
impl std::fmt::Display for PrimOp {
  #[named]
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      PrimOp::Raise { exc, expr } => write!(f, "__raise({}, {})", exc, expr),
      PrimOp::ExcTrace => write!(f, "__exctrace()"),
    }
  }
}