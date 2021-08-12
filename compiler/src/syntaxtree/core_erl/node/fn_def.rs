//! Defines a new function in Core Erlang
use crate::mfarity::MFArity;
use crate::syntaxtree::core_erl::core_ast::CoreAst;
use crate::typing::typevar::TypeVar;

/// Defines a new function in Core Erlang
pub struct FnDef {
  /// Function name/arity, module is always None
  pub funarity: MFArity,
  /// Function arguments AST, same size as arity
  pub args: Vec<CoreAst>,
  /// Function body AST, for multi-clause functions begins with a Case node.
  pub body: Box<CoreAst>,
  /// Return type variable, the meaning is assigned by the unifier
  pub ret_ty: TypeVar,
}
