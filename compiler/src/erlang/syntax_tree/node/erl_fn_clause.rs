//! Defines a FClause struct for a new function clause AST node
use std::fmt::Formatter;

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use std::sync::Arc;
use crate::display::Pretty;

/// Function clause for new function definition, collection of clauses of same arity defines
/// a new function.
pub struct ErlFnClause {
  /// Name, because it comes from AST, prefer to use funarity.name in the parent `FnDef`
  /// For inline defined lambdas name will be `None`, take care and give it a good name later
  pub name: Option<String>,
  /// Function clause arguments, binding/match expressions
  pub args: Vec<Arc<ErlAst>>,
  /// Function clause body
  pub body: Arc<ErlAst>,
  /// Guard expression, if exists
  pub guard_expr: Option<Arc<ErlAst>>,
}

impl ErlFnClause {
  /// Create a new function clause. Arguments can be any expressions.
  pub fn new(name: Option<String>, args: Vec<Arc<ErlAst>>, body: Arc<ErlAst>, guard_expr: Option<Arc<ErlAst>>) -> Self {
    ErlFnClause { name, args, body, guard_expr }
  }

  /// Returns true if all args are variables, and not expressions, i.e. accepting any value of any type
  pub fn is_all_variable_args(&self) -> bool {
    self.args.iter().all(|a| a.is_var())
  }

  // /// Return function clause type describing this single function clause arg types and return.
  // pub fn get_type(&self) -> FnClauseType {
  //   FnClauseType::new(self.arg_types.clone(), self.ret.clone())
  // }
}

impl std::fmt::Display for ErlFnClause {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{}(", self.name.clone().unwrap_or_else(|| "$unnamed-lambda".to_string()))?;
    Pretty::display_comma_separated(&self.args, f)?;
    write!(f, ") ")?;
    if let Some(gexpr) = &self.guard_expr {
      write!(f, "when {} ", *gexpr)?;
    }
    write!(f, "-> {}", self.body)
  }
}

impl std::fmt::Debug for ErlFnClause {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}(", self.name.clone().unwrap_or_else(|| "$unnamed-lambda".to_string()))?;
    Pretty::display_comma_separated(&self.args, f)?;
    write!(f, ") ")?;
    if let Some(gexpr) = &self.guard_expr {
      write!(f, "when {:?} ", *gexpr)?;
    }
    write!(f, "-> {:?}", self.body)
  }
}
