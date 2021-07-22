//! Query functions for Erlang AST trees

use crate::syntaxtree::erl::erl_ast::ErlAst;
use std::rc::Rc;

impl ErlAst {
  /// Assuming that 'self' is a root node of a module, scan the children and find a named function.
  pub fn find_fun(&self, name: &str) -> Option<Rc<ErlAst>> {
    match self {
      ErlAst::ModuleForms(forms) => {
        forms.into_iter()
            .find(|f| f.get_fun_name() == Some(name))
            .cloned()
      }
      _ => None
    }
  }
}