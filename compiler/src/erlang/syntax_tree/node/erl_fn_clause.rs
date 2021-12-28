//! Defines a FClause struct for a new function clause AST node
use std::fmt::Formatter;

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use std::sync::{Arc, RwLock, Weak};
use crate::display::Pretty;
use crate::erl_error::ErlResult;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_clause_type::FnClauseType;
use crate::typing::scope::Scope;
use crate::typing::typevar::Typevar;

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
  /// Function scope (variables and passed arguments)
  pub scope: RwLock<Scope>,
}

impl ErlFnClause {
  /// Create a new function clause. Arguments can be any expressions.
  pub fn new(name: Option<String>, args: Vec<Arc<ErlAst>>, body: Arc<ErlAst>,
             guard_expr: Option<Arc<ErlAst>>) -> Self {
    let scope_name = match &name {
      None => "unnamed fn scope".into(),
      Some(n) => n.clone(),
    };
    let variables = Default::default();
    let clause_scope = Scope::new(scope_name, Weak::new(), variables).into();
    ErlFnClause {
      name,
      args,
      body,
      guard_expr,
      scope: clause_scope,
    }
  }

  /// Returns true if all args are variables, and not expressions, i.e. accepting any value of any type
  pub fn is_all_variable_args(&self) -> bool {
    self.args.iter().all(|a| a.is_var())
  }

  /// Build `FnClauseType` from core function clause, together the clauses will form the full
  /// function type
  pub fn synthesize_clause_type(&self, scope: &RwLock<Scope>) -> ErlResult<FnClauseType> {
    // Synthesizing return type using the inner function scope, with added args
    let args_types: Vec<Typevar> = self.args.iter()
        .map(|arg| arg.synthesize(scope))
        .map(Result::unwrap)
        .map(|t| Typevar::from_erltype(&t))
        .collect();
    let synthesized_t = FnClauseType::new(
      args_types,
      self.synthesize_clause_return_type(scope)?,
    );
    Ok(synthesized_t.into())
  }

  /// Return type from the body AST
  pub fn synthesize_clause_return_type(&self, env: &RwLock<Scope>) -> ErlResult<Arc<ErlType>> {
    self.body.synthesize(env)
  }
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
