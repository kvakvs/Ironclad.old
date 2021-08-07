//! Postprocesses an AST tree. Takes a tree in, replaces some nodes, and returns a new tree.

use crate::erl_module::ErlModule;
use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::erl_error::{ErlResult};
use crate::syntaxtree::erl::node::literal_node::LiteralNode;
use crate::funarity::FunArity;
use crate::erl_module::func_registry::FunctionRegistry;

impl ErlModule {
  /// Run the post-parse analysis. Relies upon results from Self::postprocess_ast_readonly().
  /// Given a fresh parsed and processed Erlang AST, go through it once more and edit some nodes.
  /// * In function applications replace atom function names with function pointers
  pub fn postprocess_ast(ast: &mut ErlAst, env: &mut FunctionRegistry) -> ErlResult<()> {
    match ast.children_mut() {
      Some(children) => for child in children {
        Self::postprocess_ast(child, env)?;
      },
      None => {}
    }

    if let ErlAst::App(_loc, app) = ast {
      app.postprocess_edit_node(env)?;
    }
    // if let ErlAst::App(_loc1, app) = ast {
    //   if let ErlAst::Lit(_loc2, LiteralNode::Atom(fname)) = &*app.expr {
    //     let funarity = FunArity::new(fname.clone(), app.args.len());
    //   }
    // }

    Ok(())
  }
}