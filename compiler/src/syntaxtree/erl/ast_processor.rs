//! Postprocesses an AST tree. Takes a tree in, replaces some nodes, and returns a new tree.

use crate::erl_module::ErlModule;
use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::erl_error::{ErlResult};

impl ErlModule {
  /// Given a fresh parsed and processed Erlang AST, go through it once more and replace some nodes
  /// with some new nodes. Returns Ok(Some(Rc<ErlAst>)) if node must be replaced, or Ok(None) if not
  ///
  /// For example:
  /// * Trying to detect function names: ErlAst::App nodes must attempt to replace their atom
  ///   expressions with local function references, or with exports from other modules.
  pub fn postprocess_ast(&self, ast: &mut ErlAst) -> ErlResult<()> {
    for child in ast.children_mut().unwrap_or(vec![]) {
      self.postprocess_ast(child)?;
    }
    Ok(())
  }
}