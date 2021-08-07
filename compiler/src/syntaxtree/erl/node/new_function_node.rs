//! Define a FunctionDef struct for a new function AST node
use crate::syntaxtree::erl::node::fun_clause_node::FunctionClauseNode;
use crate::typing::erl_type::ErlType;
use crate::funarity::FunArity;
use crate::typing::typevar::TypeVar;
use std::fmt::Formatter;

/// AST node which declares a new function. Contains function clauses. Names and arities on
/// all clauses must be equal and same as the function name.
// #[derive(PartialEq)]
pub struct NewFunctionNode {
  /// Function name and arity, must be same for each clause (checked on clause insertion).
  pub funarity: FunArity,
  /// Index of the first function clause in order, stored sequential in `ErlModule::function_clauses`
  pub start_clause: usize,
  /// How many function clauses (more than 0)
  pub clause_count: usize,
  /// For each clause, ret is union of each clause return type
  pub ret_ty: TypeVar,
}

impl NewFunctionNode {
  /// Create a new function definition AST node. Argument types vector is initialized with unions of
  /// all argument types.
  pub fn new(funarity: FunArity, start_clause: usize, clause_count: usize) -> Self {
    assert_ne!(clause_count, 0);
    Self {
      funarity,
      start_clause,
      clause_count,
      ret_ty: TypeVar::new(),
    }
  }

  /// Readonly slice access to this function's clauses
  pub fn get_clauses<'a>(&self, clauses: &'a Vec<FunctionClauseNode>) -> &'a [FunctionClauseNode] {
    &clauses[self.start_clause..self.start_clause+self.clause_count]
  }

  /// Mutable slice access to this function's clauses
  pub fn get_clauses_mut<'a>(&self, clauses: &'a mut Vec<FunctionClauseNode>) -> &'a mut [FunctionClauseNode] {
    &mut clauses[self.start_clause..self.start_clause+self.clause_count]
  }

  /// For all clauses build a vector of type unions for the corresponding arguments
  fn get_argument_types_unions(&self, clauses: &Vec<FunctionClauseNode>) -> Vec<ErlType> {
    assert_ne!(self.clause_count, 0, "Function clauses must not be empty");

    // Assuming all clauses have same arity, build unions of each arg
    let mut arg_unions = Vec::with_capacity(self.funarity.arity);
    for _ in 0..self.funarity.arity {
      arg_unions.push(ErlType::union_empty());
    }

    // Collapse unions of 0 elements into None-type, and of 1 elements into type itself
    arg_unions = arg_unions.into_iter()
        .map(|ty| ty.union_collapse())
        .collect();

    // For each clause and for each argument in that clause, update corresponding union in arg_unions
    let own_clauses = self.get_clauses(clauses);
    own_clauses.iter().for_each(|clause| {
      // for i in 0..self.funarity.arity {
      for (i, item) in arg_unions.iter_mut().enumerate().take(self.funarity.arity) {
        assert_eq!(clause.arg_types.len(), self.funarity.arity,
                   "FunctionDef arity does not match clause arity");
        if let Some(updated) = item.union_add(&clause.arg_types[i]) {
          *item = updated;
        }
      }
    });

    arg_unions
  }
}

impl std::fmt::Debug for NewFunctionNode {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "NewFun {:?} -> {:?}", self.funarity, self.ret_ty)
  }
}