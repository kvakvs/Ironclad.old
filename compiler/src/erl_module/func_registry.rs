//! Stores functions and function clauses for an Erlang Module
use std::collections::HashMap;

use crate::funarity::FunArity;
use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::syntaxtree::erl::node::fun_clause_node::FunctionClauseNode;
use crate::syntaxtree::erl::node::literal_node::LiteralNode;
use crate::syntaxtree::erl::node::new_function_node::NewFunctionNode;

/// Stuff necessary for function registrations and function lookups
pub struct FunctionRegistry {
  /// Function definitions of the module
  pub functions: Vec<NewFunctionNode>,
  /// Each function definition has one or more clauses
  pub function_clauses: Vec<FunctionClauseNode>,
  /// Lookuip by function name and arity into `Self::functions`
  pub functions_lookup: HashMap<FunArity, usize>,
}

impl FunctionRegistry {
  /// Pushes a function node into the functions vector, updates the lookup, and returns func index
  pub fn add_function(&mut self, nf: NewFunctionNode) -> usize {
    let index = self.functions.len();
    let funarity = nf.funarity.clone();

    self.functions.push(nf);
    self.functions_lookup.insert(funarity, index);

    index
  }

  /// For an expression check whether it is a constant expression, and whether it points to some
  /// known function in this module. Arity is provided as the expression might be just an atom.
  pub fn find_function_by_expr_arity(&mut self, expr: &ErlAst, arity: usize) -> Option<usize> {
    if let ErlAst::Lit(_loc, LiteralNode::Atom(a)) = expr {
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

impl Default for FunctionRegistry {
  fn default() -> Self {
    Self {
      functions: vec![],
      function_clauses: vec![],
      functions_lookup: Default::default(),
    }
  }
}
