//! Define a FunctionDef struct for a new function AST node
use std::fmt::Formatter;

use crate::erlang::syntax_tree::node::fn_clause::FnClause;
use crate::typing::erl_type::ErlType;
use crate::mfarity::MFArity;
use crate::typing::typevar::TypeVar;

/// AST node which declares a new function. Contains function clauses. Names and arities on
/// all clauses must be equal and same as the function name.
pub struct FnDef {
  /// Function name and arity, must be same for each clause (checked on clause insertion).
  /// Always local (`MFArity::module` is `None`)
  pub mfarity: MFArity,
  /// Function clauses (non-empty vec)
  pub clauses: Vec<FnClause>,
  /// For each clause, ret is union of each clause return type
  pub ret_ty: TypeVar,
}

impl FnDef {
  /// Create a new function definition AST node. Argument types vector is initialized with unions of
  /// all argument types.
  pub fn new(funarity: MFArity, clauses: Vec<FnClause>) -> Self {
    assert!(!clauses.is_empty(), "Cannot construct a function definition without clauses");
    Self {
      mfarity: funarity,
      clauses,
      ret_ty: TypeVar::new(),
    }
  }

  /// For all clauses build a vector of type unions for the corresponding arguments
  fn get_argument_types_unions(&self) -> Vec<ErlType> {
    assert!(!self.clauses.is_empty(), "Function clauses must not be empty");

    // Assuming all clauses have same arity, build unions of each arg
    let mut arg_unions = Vec::with_capacity(self.mfarity.arity);
    for _ in 0..self.mfarity.arity {
      arg_unions.push(ErlType::union_empty());
    }

    // Collapse unions of 0 elements into None-type, and of 1 elements into type itself
    arg_unions = arg_unions.into_iter()
        .map(|ty| ty.union_collapse())
        .collect();

    // For each clause and for each argument in that clause, update corresponding union in arg_unions
    self.clauses.iter().for_each(|clause| {
      // for i in 0..self.funarity.arity {
      for (i, item) in arg_unions.iter_mut().enumerate().take(self.mfarity.arity) {
        assert_eq!(clause.arg_types.len(), self.mfarity.arity,
                   "FunctionDef arity does not match clause arity");
        if let Some(updated) = item.union_add(&clause.arg_types[i]) {
          *item = updated;
        }
      }
    });

    arg_unions
  }
}

impl std::fmt::Debug for FnDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "FnDef {:?} -> {:?}", self.mfarity, self.ret_ty)
  }
}