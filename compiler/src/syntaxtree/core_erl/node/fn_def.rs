//! Defines a new function in Core Erlang
use crate::mfarity::MFArity;
use crate::syntaxtree::core_erl::core_ast::CoreAst;

/// Defines a new function in Core Erlang
pub struct FnDef {
  /// Function name/arity, module is always None
  pub name: MFArity,
  /// Function body AST, for multi-clause functions begins with a Case node.
  pub body: Box<CoreAst>,
}
