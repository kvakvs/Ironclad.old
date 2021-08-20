//! A function call to a local fn or an exported fn from another module

use crate::mfarity::MFArity;
use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::typing::typevar::TypeVar;

/// Contains a function call on a MFArity
pub struct Call {
  /// Must exist
  pub target: MFArity,
  /// Must match arity
  pub args: Vec<CoreAst>,
  /// The unique typevar for return type
  pub ret_ty: TypeVar,
}
