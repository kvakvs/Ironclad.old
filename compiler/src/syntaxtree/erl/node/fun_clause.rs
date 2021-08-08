//! Defines a FClause struct for a new function clause AST node
use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::typing::erl_type::ErlType;

/// Function clause for new function definition, collection of clauses of same arity defines
/// a new function.
#[derive(Debug)]
pub struct FunctionClause {
  /// Name, because it comes from AST, prefer to use funarity.name in the parent `FunctionDef`
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

impl FunctionClause {
  /// Create a new function clause
  pub fn new(name: String, args: Vec<ErlAst>, body: ErlAst) -> Self {
    let arg_types = args.iter()
        .map(|_a| ErlType::new_typevar())
        .collect();
    FunctionClause {
      name,
      args,
      arg_types,
      body: Box::new(body),
      ret: ErlType::new_typevar(),
    }
  }
}