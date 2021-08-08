//! Stores functions and function clauses for an Erlang Module
use crate::funarity::FunArity;
use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::syntaxtree::erl::node::literal_node::Literal;
use crate::syntaxtree::erl::node::function_def::FunctionDef;
use crate::erl_module::ErlModule;

impl ErlModule {
  /// Pushes a function node into the functions vector, updates the lookup, and returns func index
  pub fn add_function(&mut self, nf: FunctionDef) -> usize {
    let index = self.functions.len();
    let funarity = nf.funarity.clone();

    self.functions.push(nf);
    self.functions_lookup.insert(funarity, index);

    index
  }

  /// For an expression check whether it is a constant expression, and whether it points to some
  /// known function in this module. Arity is provided as the expression might be just an atom.
  pub fn find_function_by_expr_arity(&self, expr: &ErlAst, arity: usize) -> Option<usize> {
    if let ErlAst::Lit(_loc, Literal::Atom(a)) = expr {
      // A single atom points to a possible existing function of `arity` in the current module
      let fa = FunArity { name: a.clone(), arity };
      match self.functions_lookup.get(&fa) {
        None => return None,
        Some(index) => return Some(*index),
      }
    }
    None
  }
}

// impl Default for FunctionRegistry {
//   fn default() -> Self {
//     Self {
//       functions: vec![],
//       function_clauses: vec![],
//       functions_lookup: Default::default(),
//     }
//   }
// }
