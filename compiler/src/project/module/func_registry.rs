//! Stores functions and function clauses for an Erlang Module
use std::sync::Arc;

use crate::mfarity::MFArity;
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::literal::Literal;
use crate::core_erlang::syntax_tree::node::fn_def::FnDef;
use std::ops::Deref;
use std::collections::HashMap;

/// Collection of module functions and a lookup table
pub struct FuncRegistry {
  /// Function definitions of the module
  pub functions: Vec<Arc<FnDef>>,

  /// Lookup by function_name/arity into `Self::functions`
  pub functions_lookup: HashMap<MFArity, usize>,
}

impl Default for FuncRegistry {
  fn default() -> Self {
    FuncRegistry {
      functions: vec![],
      functions_lookup: Default::default(),
    }
  }
}

impl FuncRegistry {
  /// Pushes a function node into the functions vector, updates the lookup, and returns func index
  pub fn add_function(&mut self, nf: Arc<FnDef>) {
    let index = self.functions.len();
    let funarity = nf.funarity.clone();

    self.functions.push(nf);
    self.functions_lookup.insert(funarity, index);
  }

  /// For an expression check whether it is a constant expression, and whether it points to some
  /// known function in this module. Arity is provided as the expression might be just an atom.
  pub fn find_function_by_expr_arity(&self, expr: &ErlAst, arity: usize) -> Option<usize> {
    if let ErlAst::Lit { value: lit, .. } = expr {
      if let Literal::Atom(a) = lit.deref() {
        // A single atom points to a possible existing function of `arity` in the current module
        let mfa = MFArity { module: None, name: a.clone(), arity };
        return self.functions_lookup.get(&mfa).copied();
      }
    }
    None
  }
}
