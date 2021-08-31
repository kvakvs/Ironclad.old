//! A function call to a local fn or an exported fn from another module
use std::sync::Arc;

use crate::mfarity::MFArity;
use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::typing::typevar::TypeVar;
use crate::source_loc::SourceLoc;

/// Contains a function call on a MFArity
#[derive(Debug)]
pub struct Call {
  /// Source file pointer
  location: SourceLoc,
  /// Must exist
  pub target: MFArity,
  /// Must match arity
  pub args: Vec<Arc<CoreAst>>,
  /// The unique typevar for return type
  pub ret_ty: TypeVar,
}
