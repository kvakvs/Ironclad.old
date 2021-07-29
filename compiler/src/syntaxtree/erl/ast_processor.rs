//! Postprocesses an AST tree. Takes a tree in, replaces some nodes, and returns a new tree.

use crate::erl_module::ErlModule;
use std::rc::Rc;
use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::erl_error::{ErlResult, ErlError};
use std::ops::Deref;

impl ErlModule {
  /// Given a fresh parsed and processed Erlang AST, go through it once more and replace some nodes
  /// with some new nodes. Returns Ok(Some(Rc<ErlAst>)) if node must be replaced, or Ok(None) if not
  ///
  /// For example:
  /// * Application AST nodes must attempt to replace their atom expressions with
  ///   local function references, or with exports from other modules.
  // fn postprocess_ast(&self, ast: Rc<ErlAst>) -> ErlResult<Rc<ErlAst>> {
  //   // Recursively descend into AST and visit deepest nodes first
  //   match self.postprocess_edit_node(ast)? {
  //     Some(new_node) => Ok(new_node),
  //     None => {}
  //   }
  // }
  //
  // /// Based on the node contents maybe replace the node or its parts, and maybe return a new node,
  // /// or return old node, if nothing changed. Returns Some() to replace, or None to drop.
  pub fn postprocess_ast(&self, ast: Rc<ErlAst>) -> ErlResult<Option<Rc<ErlAst>>> {
    match ast.deref() {
      ErlAst::Empty => Ok(None),
      ErlAst::Comment => Ok(None),
      ErlAst::Token(_) => {
        Err(ErlError::parser_internal(
          ast.clone(),
          format!("Stray token must be eliminated from AST at this stage: {:?}", ast)))
      }
      ErlAst::ModuleForms(forms) => {
        let new_forms: Vec<Rc<ErlAst>> = forms.iter()
            .filter_map(|f| {
              self.postprocess_ast(f.clone()).unwrap()
            })
            .collect();
        let new_node = Rc::new(ErlAst::ModuleForms(new_forms));
        Ok(Some(new_node))
      }
      // ErlAst::ModuleAttr { .. } => {}
      // ErlAst::Comma { .. } => {}
      // ErlAst::NewFunction(_) => {}
      // ErlAst::FClause(_) => {}
      // ErlAst::CClause(_) => {}
      // ErlAst::Var(_) => {}
      // ErlAst::App(_) => {}
      // ErlAst::Let(_) => {}
      // ErlAst::Case(_) => {}
      // ErlAst::Lit(_) => {}
      // ErlAst::BinaryOp(_) => {}
      // ErlAst::UnaryOp(_) => {}
      _ => Err(ErlError::parser_internal(
        ast.clone(),
        format!("Postprocess AST doesn't know how to handle node: {:?}", ast)))
    }
  }
}