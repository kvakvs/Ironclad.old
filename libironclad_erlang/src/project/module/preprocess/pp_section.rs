//! Section of a module, opened when preprocessor interpreter has encountered an if condition
//! or an else directive. Closes when endif is encountered.

use crate::erl_syntax::preprocessor::pp_node::PreprocessorNode;

/// An open -if/ifdef/ifndef section
pub(crate) struct PreprocessorSection {
  /// The node which triggered the new section
  pub(crate) ppnode: PreprocessorNode,
  /// Whether the condition in this section is true (to emit the preprocessed tokens into the output)
  pub(crate) condition: bool,
  /// To prevent double else
  pub(crate) else_encountered: bool,
}

impl PreprocessorSection {
  /// Create new section
  pub(crate) fn new(ppnode: PreprocessorNode, is_true: bool) -> Self {
    PreprocessorSection {
      ppnode,
      condition: is_true,
      else_encountered: false,
    }
  }
}
