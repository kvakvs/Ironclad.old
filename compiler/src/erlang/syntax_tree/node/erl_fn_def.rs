//! Define a ErlFnDef struct for a new function AST node
use std::fmt::Formatter;

use crate::erlang::syntax_tree::node::erl_fn_clause::ErlFnClause;
use crate::mfarity::MFArity;
use crate::source_loc::SourceLoc;

/// AST node which declares a new function. Contains function clauses. Names and arities on
/// all clauses must be equal and same as the function name.
pub struct ErlFnDef {
  /// Source file pointer
  pub location: SourceLoc,
  /// Function name and arity, must be same for each clause (checked on clause insertion).
  /// Always local (`MFArity::module` is `None`)
  pub funarity: MFArity,
  /// Function clauses (non-empty vec)
  pub clauses: Vec<ErlFnClause>,
}

impl ErlFnDef {
  /// Create a new function definition AST node. Argument types vector is initialized with unions of
  /// all argument types.
  pub fn new(location: SourceLoc, funarity: MFArity, clauses: Vec<ErlFnClause>) -> Self {
    assert!(!clauses.is_empty(), "Cannot construct a function definition without clauses");
    Self {
      location,
      funarity,
      clauses,
    }
  }

  // /// For all clauses build a vector of type unions for the corresponding arguments
  // fn get_argument_types_unions(&self) -> Vec<ErlType> {
  //   assert!(!self.clauses.is_empty(), "Function clauses must not be empty");
  //
  //   // Assuming all clauses have same arity, build unions of each arg
  //   let mut arg_unions = Vec::with_capacity(self.funarity.arity);
  //   for _ in 0..self.funarity.arity {
  //     arg_unions.push(ErlType::union_empty());
  //   }
  //
  //   // Collapse unions of 0 elements into None-type, and of 1 elements into type itself
  //   arg_unions = arg_unions.into_iter()
  //       .map(|ty| ty.union_collapse())
  //       .collect();
  //
  //   // For each clause and for each argument in that clause, update corresponding union in arg_unions
  //   self.clauses.iter().for_each(|clause| {
  //     // for i in 0..self.funarity.arity {
  //     for (i, item) in arg_unions.iter_mut().enumerate().take(self.funarity.arity) {
  //       assert_eq!(clause.args.len(), self.funarity.arity,
  //                  "FunctionDef arity does not match clause arity");
  //       if let Some(updated) = item.union_add(&clause.arg_types[i]) {
  //         *item = updated;
  //       }
  //     }
  //   });
  //
  //   arg_unions
  // }
}

impl std::fmt::Debug for ErlFnDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "FnDef {:?}", self.funarity)
  }
}