//! Query functions for Erlang AST trees

use crate::syntaxtree::erl::erl_ast::ErlAst;
use std::rc::Rc;
use crate::syntaxtree::erl::new_function::NewFunction;
use std::ops::Deref;

/// Returned from find_fun. Contains references, if you need to use them in the code and drop the
/// borrowed parent, use .clone()
pub struct FindFunResult<'a> {
  /// Reference to AST Rc pointer where the function was found. Use .clone()
  /// if you need to retain it.
  pub ast: &'a Rc<ErlAst>,
  /// Reference to a NewFunction struct in the AST node which was found. Use .clone() if you
  /// need to retain it.
  pub(crate) new_function: &'a NewFunction,
}

impl ErlAst {
  /// Assuming that 'self' is a root node of a module, scan the children and find a named function.
  /// Returns a FindFunResult with a pointer to AST where this was found, and the reference to
  /// NewFunction in that AST node (clone if necessary).
  pub fn find_fun(&self, name: &str, arity: usize) -> Option<FindFunResult> {
    match self {
      ErlAst::ModuleForms(forms) => {
        forms.into_iter()
            .find(|each_form| {
              if let ErlAst::NewFunction(nf) = (*each_form).deref() {
                return nf.arity == arity
                    && nf.clauses[0].name == name;
              }
              false
            })
            .map(|rc| {
              FindFunResult {
                ast: rc,
                new_function: rc.as_new_function().unwrap(),
              }
            })
      }
      _ => None
    }
  }
}