//! Defines a FClause struct for a new function clause AST node
use std::rc::Rc;

use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::typing::erl_type::ErlType;

/// Function clause for new function definition, collection of clauses of same arity defines
/// a new function.
#[derive(PartialEq)]
pub struct FunctionClauseNode {
  /// Function name atom, stored as a string. All clauses of the same function must have same name
  pub name: String,
  /// Function clause arguments, binding/match expressions
  pub args: Vec<ErlAst>,
  /// Types we believe the arguments will have
  pub arg_types: Vec<ErlType>,
  /// Function clause body
  pub body: Box<ErlAst>,
  /// Return type for this function clause
  pub ret: ErlType,
}

impl FunctionClauseNode {
  /// Create a new function clause
  pub fn new(name: String, args: Vec<ErlAst>, body: ErlAst) -> Self {
    let arg_types = args.iter()
        .map(|_a| ErlType::new_typevar())
        .collect();
    FunctionClauseNode {
      name,
      args,
      arg_types,
      body: Box::new(body),
      ret: ErlType::new_typevar(),
    }
  }
}
