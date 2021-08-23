//! Define a ErlFnDef struct for a new function AST node
use crate::erlang::syntax_tree::node::erl_fn_clause::ErlFnClause;
use crate::mfarity::MFArity;
use crate::source_loc::SourceLoc;

/// AST node which declares a new function. Contains function clauses. Names and arities on
/// all clauses must be equal and same as the function name.
#[derive(Debug)]
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
}
