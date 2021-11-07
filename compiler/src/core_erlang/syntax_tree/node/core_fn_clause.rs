//! Defines core function clause

use std::fmt::Formatter;
use std::sync::{Arc, RwLock};
use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::core_erlang::syntax_tree::node::var::Var;
use crate::display::Pretty;
use crate::erl_error::ErlResult;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_clause_type::FnClauseType;
use crate::typing::scope::Scope;

/// Core function clause.
/// We keep the function clauses separate and split without merging it into one big case operator,
/// for type checking purposes.
#[derive(Debug)]
pub struct CoreFnClause {
  /// Argument match expressions directly translated from `ErlAst`
  args_ast: Vec<Arc<CoreAst>>,
  /// Argument variable names (named as incoming into the function)
  args: Vec<Arc<Var>>,
  /// Function clause body directly translated from `ErlAst`
  body: Arc<CoreAst>,
  /// The optional guard expression
  guard: Option<Arc<CoreAst>>,
  /// Function scope (variables and passed arguments)
  pub scope: Arc<RwLock<Scope>>,
}

impl CoreFnClause {
  /// Creates a new Core Function Clause
  /// All input variables are given unique new names.
  pub fn new(fn_header_scope: &Arc<RwLock<Scope>>,
             args_ast: Vec<Arc<CoreAst>>,
             body: Arc<CoreAst>,
             guard: Option<Arc<CoreAst>>) -> Self {
    // let args: Vec<Arc<Var>> = args_ast.iter()
    //     .map(|arg_ast| ExtractVar::extract_vars(arg_ast))
    //     .flatten()
    //     .collect();
    // println!("New fnclause: args {:?}\nnew args {:?}", args_ast, args);
    let args: Vec<Arc<Var>> = args_ast.iter()
        .map(|ast| Var::new_unique(ast.location(), "Arg").into())
        .collect();

    // Create inner_env for each arg where it has any() type, later this can be amended
    let inner_scope: Scope = args.iter()
        .fold(Scope::empty(Arc::downgrade(fn_header_scope)),
              |scope, arg| scope.add(&arg));
    Self {
      args,
      args_ast,
      body,
      guard,
      scope: inner_scope.into_arc_rwlock(),
    }
  }

  /// Read-only access to args
  pub fn args(&self) -> &Vec<Arc<Var>> { &self.args }

  /// Read-only access to body Arc (clone if you have to)
  pub fn body(&self) -> &Arc<CoreAst> { &self.body }

  /// Read-only access to guard expression (clone if you have to)
  pub fn guard(&self) -> &Option<Arc<CoreAst>> { &self.guard }

  /// Build `FnClauseType` from core function clause, together the clauses will form the full
  /// function type
  pub fn synthesize_clause_type(&self, scope: &Arc<RwLock<Scope>>) -> ErlResult<Arc<FnClauseType>> {
    // Synthesizing return type using the inner function scope, with added args
    let args_types = self.args.iter()
        .map(|_| Var::synthesize_type())
        .collect();
    let synthesized_t = FnClauseType::new(
      args_types,
      self.synthesize_clause_return_type(scope)?,
    );
    Ok(synthesized_t.into())
  }

  /// Return type from the body AST
  pub fn synthesize_clause_return_type(&self, env: &Arc<RwLock<Scope>>) -> ErlResult<Arc<ErlType>> {
    self.body.synthesize_type(env)
  }
}

impl std::fmt::Display for CoreFnClause {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    Pretty::display_paren_list(&self.args, f)?;
    match &self.guard {
      Some(g) => write!(f, " when {}", g)?,
      None => {}
    }
    write!(f, " -> {}", self.body)
  }
}
