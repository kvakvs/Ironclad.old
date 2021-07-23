//! Define a NewFunction struct for a new function AST node
use crate::syntaxtree::erl::fclause::FClause;
use crate::typing::erl_type::ErlType;

/// AST node which declares a new function. Contains function clauses. Names and arities on
/// all clauses must be equal and same as the function name.
#[derive(PartialEq, Clone)]
pub struct NewFunction {
  /// Function arity, must be same for each clause
  pub arity: usize,
  /// Argument types where each is a union of each function clause
  pub arg_ty: Vec<ErlType>,
  /// Function clauses in order
  pub clauses: Vec<FClause>,
  /// For each clause, ret is union of each clause return type
  pub ret: ErlType,
}

impl NewFunction {
  /// Create a new function definition AST node. Argument types vector is initialized with unions of
  /// all argument types.
  pub fn new(arity: usize, clauses: Vec<FClause>, ret: ErlType) -> Self {
    let mut result = Self {
      arity,
      clauses,
      ret,
      arg_ty: vec![],
    };
    result.arg_ty = result.get_argument_types_unions();
    result
  }

  /// For all clauses build a vector of type unions for the corresponding arguments
  fn get_argument_types_unions(&self) -> Vec<ErlType> {
    assert!(self.clauses.len() > 0, "Function clauses must not be empty");
    // Assuming all clauses have same arity, build unions of each arg
    let mut arg_unions = Vec::with_capacity(self.arity);
    for _ in 0..self.arity {
      arg_unions.push(ErlType::Union(vec![]));
    }

    // Collapse unions of 0 elements into None-type, and of 1 elements into type itself
    arg_unions = arg_unions.into_iter()
        .map(|ty| ty.union_collapse())
        .collect();

    // For each clause and for each argument in that clause, update corresponding union in arg_unions
    self.clauses.iter().for_each(|clause| {
      for i in 0..self.arity {
        assert_eq!(clause.arg_types.len(), self.arity, "NewFunction arity does not match clause arity");
        match arg_unions[i].union_add(&clause.arg_types[i]) {
          Some(updated) => arg_unions[i] = updated,
          None => {}
        }
      }
    });

    arg_unions
  }
}