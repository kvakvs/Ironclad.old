//! Defines primitive operations for BEAM assembly and BEAM VM, which are not part of Core Erlang.

use crate::syntaxtree::core_erl::core_ast::CoreAst;

pub enum PrimExcType {
  Error,
  Exit,
  Throw,
}

pub struct PrimRaise {
  pub exc: PrimExcType,
  pub expr: Box<CoreAst>,
}

/// Primitive operation, not part of Core Erlang language but is useful to express Erlang constructs
pub enum PrimOp {
  Raise(PrimRaise),
  // primop exc_trace
  ExcTrace,
}
