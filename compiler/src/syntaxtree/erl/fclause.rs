//! Defines a FClause struct for a new function clause AST node
use std::rc::Rc;

use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::typing::erl_type::ErlType;

/// Function clause for new function definition, collection of clauses of same arity defines
/// a new function.
#[derive(PartialEq, Clone)]
pub struct FClause {
  /// Function name atom, stored as a string. All clauses of the same function must have same name
  pub name: String,
  /// Function clause arguments, binding/match expressions
  pub args: Vec<Rc<ErlAst>>,
  /// Types we believe the arguments will have
  pub arg_types: Vec<ErlType>,
  /// Function clause body
  pub body: Rc<ErlAst>,
  /// Return type for this function clause
  pub ret: ErlType,
}

impl FClause {
  /// Create a new function clause
  pub fn new(name: &str, args: Vec<Rc<ErlAst>>, body: Rc<ErlAst>) -> Self {
    let arg_types = args.iter()
        .map(|_a| ErlType::new_typevar())
        .collect();
    FClause {
      name: name.to_string(),
      args,
      arg_types,
      body,
      ret: ErlType::new_typevar(),
    }
  }
}
