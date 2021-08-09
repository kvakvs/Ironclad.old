//! Defines function type describing a function with clauses where each clause has arguments
use crate::typing::erl_type::ErlType;
use crate::typing::fn_clause_type::FnClauseType;
use std::fmt::Formatter;
use crate::display::display_semicolon_separated;

/// ErlType variant for a function or a lambda
#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct FunctionType {
  /// Name if known, for module level functions, or unnamed for anonymous funs
  pub name: Option<String>,
  /// Function arity
  pub arity: usize,
  /// Multiple function clauses, return type and arguments are independent, only held together by
  /// having same arity.
  pub clauses: Vec<FnClauseType>,
  /// Union of all clause return types
  pub ret_type: Box<ErlType>,
}

impl FunctionType {
  /// Create a new function type from its optional name and function clauses (min. 1 clause)
  pub fn new(name: Option<String>, clauses: Vec<FnClauseType>) -> Self {
    assert!(!clauses.is_empty(), "Function type with 0 function clause types is not allowed");
    let arity = clauses[0].arg_types.len();

    assert!(clauses.iter().all(|c| c.arg_types.len() == arity));
    let ret_type = Self::build_compound_ret_type(&clauses);
    Self { name, arity, clauses, ret_type }
  }

  /// Merge return types from all function clauses.
  /// Call this when `self.clauses` are updated and `ret_type` needs to also be updated.
  pub fn build_compound_ret_type(clauses: &[FnClauseType]) -> Box<ErlType> {
    let all_ret_types = clauses.iter()
        .map(|fct| *fct.ret_ty.clone())
        .collect();
    Box::new(ErlType::union_of(all_ret_types, true))
  }
}

impl std::fmt::Display for FunctionType {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "fun(")?;
    display_semicolon_separated(&self.clauses, f)?;
    write!(f, ")")
  }
}