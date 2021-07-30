//! Postprocesses an AST tree. Takes a tree in, replaces some nodes, and returns a new tree.

use crate::erl_module::ErlModule;
use std::rc::Rc;
use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::erl_error::{ErlResult, ErlError};
use std::ops::Deref;
use crate::syntaxtree::erl::node::t_postprocess::TPostProcess;

impl ErlModule {
  /// Given a fresh parsed and processed Erlang AST, go through it once more and replace some nodes
  /// with some new nodes. Returns Ok(Some(Rc<ErlAst>)) if node must be replaced, or Ok(None) if not
  ///
  /// For example:
  /// * Trying to detect function names: ErlAst::App nodes must attempt to replace their atom
  ///   expressions with local function references, or with exports from other modules.
  pub fn postprocess_ast(&mut self, ast: &mut ErlAst) -> ErlResult<()> {
    match ast.get_children() {
      None => {}
      Some(children) => {
        let errors: Vec<ErlError> = children.into_iter()
            .map(|child| self.postprocess_ast(child))
            .filter(Result::is_err)
            .map(Result::err)
            .map(Option::unwrap)
            .collect();
        if !errors.is_empty() {
          return Err(ErlError::Multiple(errors))
        }
      }
    }
    // match ast {
    //   ErlAst::Empty | ErlAst::Comment { .. } => {},
    //   ErlAst::Token { .. } => {
    //     Err(ErlError::parser_internal(
    //       Some(ast.location()),
    //       format!("Stray token must be eliminated from AST at this stage: {:?}", ast)))
    //   }
    //   ErlAst::ModuleForms(forms) => {
    //     let new_forms: Vec<Rc<ErlAst>> = forms.iter()
    //         .filter_map(|f| {
    //           self.postprocess_ast(f.clone()).unwrap()
    //         })
    //         .collect();
    //     let new_node = Rc::new(ErlAst::ModuleForms(new_forms));
    //   }
    //   // ErlAst::ModuleAttr { .. } => {}
    //   // ErlAst::Comma { .. } => {}
    //   // ErlAst::NewFunction(_) => {}
    //   // ErlAst::FClause(_) => {}
    //   // ErlAst::CClause(_) => {}
    //   // ErlAst::Var(_) => {}
    //   ErlAst::App(_loc, app) => app.postprocess_ast(self)?,
    //   // ErlAst::Let(_) => {}
    //   // ErlAst::Case(_) => {}
    //   // ErlAst::Lit(_) => {}
    //   // ErlAst::BinaryOp(_) => {}
    //   // ErlAst::UnaryOp(_) => {}
    //   _ => Err(ErlError::parser_internal(
    //     Some(ast.location()),
    //     format!("Postprocess AST doesn't know how to handle node: {:?}", ast)))
    // }
    Ok(())
  }
}