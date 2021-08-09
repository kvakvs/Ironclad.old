//! Defines a FClause struct for a new function clause AST node
use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::typing::erl_type::ErlType;
use std::fmt::Formatter;
use crate::display;

/// Function clause for new function definition, collection of clauses of same arity defines
/// a new function.
pub struct FnClause {
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

impl FnClause {
  /// Create a new function clause
  pub fn new(name: String, args: Vec<ErlAst>, body: ErlAst) -> Self {
    let arg_types = args.iter()
        .map(|_a| ErlType::new_typevar())
        .collect();
    FnClause {
      name,
      args,
      arg_types,
      body: Box::new(body),
      ret: ErlType::new_typevar(),
    }
  }
}

impl std::fmt::Display for FnClause {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{}(", self.name)?;
    display::display_comma_separated(&self.args, f)?;
    write!(f, ") -> {}", self.body)
  }
}

impl std::fmt::Debug for FnClause {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{} -> {}", self, self.ret)
  }
}
