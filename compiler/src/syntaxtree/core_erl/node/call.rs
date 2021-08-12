//! A function call to a local fn or an exported fn from another module

use crate::mfarity::MFArity;
use crate::syntaxtree::core_erl::core_ast::CoreAst;

/// Contains a function call on a MFArity
pub struct Call {
  /// Must exist
  pub target: MFArity,
  /// Must match arity
  pub args: Vec<CoreAst>,
}
