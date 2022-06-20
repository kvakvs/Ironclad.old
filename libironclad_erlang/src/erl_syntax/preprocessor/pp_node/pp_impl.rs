//! Preprocessor node wrapper struct

use crate::erl_syntax::preprocessor::pp_node::pp_type::PreprocessorNodeType;
use crate::source_loc::SourceLoc;

/// Groups location and the preprocessor node type
#[derive(Debug)]
pub struct PreprocessorNodeImpl {
  /// Where encountered
  pub location: SourceLoc,
  /// The node type and content
  pub content: PreprocessorNodeType,
}
