//! Defines a FClause struct for a new function clause AST node
use std::fmt::Formatter;

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::typing::erl_type::ErlType;
use crate::display;
use crate::typing::fn_clause_type::FnClauseType;
use std::sync::Arc;

/// Function clause for new function definition, collection of clauses of same arity defines
/// a new function.
pub struct ErlFnClause {
  /// Name, because it comes from AST, prefer to use funarity.name in the parent `FunctionDef`
  pub name: String,
  /// Function clause arguments, binding/match expressions
  pub args: Vec<Arc<ErlAst>>,
  /// Types we believe the arguments will have
  pub arg_types: Vec<ErlType>,
  /// Function clause body
  pub body: Arc<ErlAst>,
  /// Return type for this function clause
  pub ret: ErlType,
}

impl ErlFnClause {
  /// Create a new function clause. Arguments can be any expressions.
  pub fn new(name: String, args: Vec<Arc<ErlAst>>, body: ErlAst) -> Self {
    let arg_types = args.iter()
        .map(|_a| ErlType::new_typevar())
        .collect();
    ErlFnClause {
      name,
      args,
      arg_types,
      body: Arc::new(body),
      ret: ErlType::new_typevar(),
    }
  }

  /// Return function clause type describing this single function clause arg types and return.
  pub fn get_type(&self) -> FnClauseType {
    FnClauseType::new(self.arg_types.clone(), self.ret.clone())
  }
}

impl std::fmt::Display for ErlFnClause {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{}(", self.name)?;
    display::display_comma_separated(&self.args, f)?;
    write!(f, ") -> {}", self.body)
  }
}

impl std::fmt::Debug for ErlFnClause {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}(", self.name)?;
    display::display_comma_separated(&self.args, f)?;
    write!(f, "):{} -> {:?}", self.ret, self.body)
  }
}
