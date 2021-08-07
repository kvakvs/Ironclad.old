//! Postprocesses an AST tree. Takes a tree in, replaces some nodes, and returns a new tree.

use crate::erl_module::ErlModule;
use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::erl_error::{ErlResult};
use crate::syntaxtree::erl::node::t_postprocess::TPostProcess;

impl ErlModule {
  /// Given a fresh parsed and processed Erlang AST, go through it once more and replace some nodes
  /// with some new nodes. Returns Ok(Some(Rc<ErlAst>)) if node must be replaced, or Ok(None) if not
  ///
  /// For example:
  /// * Trying to detect function names: ErlAst::App nodes must attempt to replace their atom
  ///   expressions with local function references, or with exports from other modules.
  pub fn postprocess_ast(&mut self, ast: &mut ErlAst) -> ErlResult<()> {
    // println!("Postprocessing node... {}", ast);

    match ast.children_mut() {
      Some(children) => for child in children {
        self.postprocess_ast(child)?;
      },
      None => {}
    }

    if let ErlAst::App(_loc, app) = ast {
      app.postprocess_ast(self)?;
    }

    Ok(())
  }
}