//! Defines a FClause struct for a new function clause AST node
use std::fmt::Formatter;

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::error::ic_error::IcResult;
use crate::project::module::mod_impl::ErlModule;
use crate::project::module::scope::scope_impl::{Scope, ScopeImpl};
use crate::typing::erl_type::ErlType;
use crate::typing::fn_clause_type::FnClauseType;
use crate::typing::typevar::Typevar;
use libironclad_util::pretty::Pretty;
use std::sync::Weak;

/// Function clause for new function definition, collection of clauses of same arity defines
/// a new function.
pub struct ErlFnClause {
  /// Name, because it comes from AST, prefer to use funarity.name in the parent `FnDef`
  /// For inline defined lambdas name will be `None`, take care and give it a good name later
  pub name: Option<String>,
  /// Function clause arguments, binding/match expressions
  pub args: Vec<AstNode>,
  /// Function clause body: a comma-expression which is a vec of expressions, and result is the last one
  pub body: AstNode,
  /// Guard expression, if exists
  pub guard_expr: Option<AstNode>,
  /// Function scope (variables and passed arguments)
  pub scope: Scope,
}

impl ErlFnClause {
  /// Create a new function clause. Arguments can be any expressions.
  pub(crate) fn new(
    name: Option<String>,
    args: Vec<AstNode>,
    body: AstNode,
    guard_expr: Option<AstNode>,
  ) -> Self {
    // TODO: For root level function defs assert that name is Some. For lambdas: that name is None
    let scope_name = match &name {
      None => "unnamed fn scope".into(),
      Some(n) => n.clone(),
    };

    // Extract all bindable variables which introduce a new variable name in the scope
    let mut variables = Default::default();
    for arg in &args {
      AstNodeImpl::extract_variables(arg, &mut variables).unwrap();
    }

    let clause_scope = ScopeImpl::new(scope_name, Weak::new(), variables);

    ErlFnClause { name, args, body, guard_expr, scope: clause_scope }
  }

  /// Returns true if all args are variables, and not expressions, i.e. accepting any value of any type
  #[allow(dead_code)]
  pub(crate) fn is_all_variable_args(&self) -> bool {
    self.args.iter().all(|a| a.is_var())
  }

  /// Build `FnClauseType` from core function clause, together the clauses will form the full
  /// function type
  #[allow(dead_code)]
  pub(crate) fn synthesize_clause_type(
    &self,
    module: &ErlModule,
    scope: &Scope,
  ) -> IcResult<FnClauseType> {
    // Synthesizing return type using the inner function scope, with added args
    // let args_types: Vec<Typevar> = self.args.iter()
    //     .map(|arg| arg.synthesize(scope))
    //     .map(Result::unwrap)
    //     .map(|t| Typevar::from_erltype(&t))
    //     .collect();
    let mut args_types = Vec::new();
    for arg in &self.args {
      let synth = arg.synthesize(module, scope)?;
      args_types.push(Typevar::from_erltype(&synth));
    }

    let synthesized_t = FnClauseType::new(
      args_types,
      Typevar::from_erltype(&self.synthesize_clause_return_type(module, scope)?),
    );
    Ok(synthesized_t)
  }

  /// Return type from the body AST
  #[allow(dead_code)]
  pub(crate) fn synthesize_clause_return_type(
    &self,
    module: &ErlModule,
    env: &Scope,
  ) -> IcResult<ErlType> {
    self.body.synthesize(module, env)
  }
}

impl std::fmt::Display for ErlFnClause {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(
      f,
      "{}(",
      self
        .name
        .clone()
        .unwrap_or_else(|| "$unnamed-lambda".to_string())
    )?;
    Pretty::display_comma_separated(self.args.iter(), f)?;
    write!(f, ") ")?;
    if let Some(gexpr) = &self.guard_expr {
      write!(f, "when {} ", *gexpr)?;
    }
    write!(f, "-> {}", self.body)
  }
}

impl std::fmt::Debug for ErlFnClause {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}(",
      self
        .name
        .clone()
        .unwrap_or_else(|| "$unnamed-lambda".to_string())
    )?;
    Pretty::display_comma_separated(self.args.iter(), f)?;
    write!(f, ") ")?;
    if let Some(gexpr) = &self.guard_expr {
      write!(f, "when {:?} ", *gexpr)?;
    }
    write!(f, "-> {:?}", self.body)
  }
}
