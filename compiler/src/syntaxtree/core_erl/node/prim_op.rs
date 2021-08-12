//! Defines primitive operations for BEAM assembly and BEAM VM, which are not part of Core Erlang.

use crate::syntaxtree::core_erl::core_ast::CoreAst;

/// Describes an exception kind
pub enum ExceptionType {
  /// `erlang:error`
  Error,
  /// `erlang:exit`
  Exit,
  /// `erlang:throw`
  Throw,
}

/// Primitive operation, not part of Core Erlang language but is useful to express Erlang constructs
pub enum PrimOp {
  /// Raises an exception of type
  Raise {
    /// Exception kind
    exc: ExceptionType,
    /// The value to be thrown
    expr: Box<CoreAst>,
  },
  /// primop `exc_trace`
  ExcTrace,
}
