//! Defines function type describing a function with clauses where each clause has arguments
use crate::typing::erl_type::ErlType;
use crate::typing::function_clause_type::FunctionClauseType;
use std::fmt::Formatter;
use crate::display::display_semicolon_separated;

/// ErlType variant for a function or a lambda
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct FunctionType {
  /// Name if known, for module level functions, or unnamed for anonymous funs
  pub name: Option<String>,
  /// Function arity
  pub arity: usize,
  /// Multiple function clauses, return type and arguments are independent, only held together by
  /// having same arity.
  pub clauses: Vec<FunctionClauseType>,
  /// Union of all clause return types
  pub ret_type: Box<ErlType>,
}

impl FunctionType {
  pub fn new(name: Option<String>, clauses: Vec<FunctionClauseType>) -> Self {
    assert!(!clauses.is_empty(), "Function type with 0 function clause types is not allowed");
    let arity = clauses[0].args.len();

    assert!(clauses.iter().all(|c| c.args.len() == arity));

    let all_ret_types = clauses.iter()
        .map(|fct| fct.ret_ty.clone())
        .collect();

    Self {
      name,
      arity,
      clauses,
      ret_type: Box::new(ErlType::Union(all_ret_types)),
    }
  }
}

impl std::fmt::Display for FunctionType {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "fun(")?;
    display_semicolon_separated(self.clauses);
    write!(f, ")")
  }
}