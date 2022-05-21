//! Defines core function clause
#![cfg(coreast)]

use std::fmt::Formatter;
use std::sync::{Arc, RwLock};
use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::display::Pretty;
use crate::erl_error::ErlResult;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_clause_type::FnClauseType;
use crate::typing::scope::Scope;
use crate::typing::typevar::Typevar;

/// Core function clause.
/// We keep the function clauses separate and split without merging it into one big case operator,
/// for type checking purposes.
#[cfg(coreast)]
#[derive(Debug)]
pub struct CoreFnClause {
  /// Argument match expressions directly translated from `ErlAst`
  pub args_ast: Vec<Arc<CoreAst>>,
  /// Function clause body directly translated from `ErlAst`
  pub body: Arc<CoreAst>,
  /// The optional guard expression
  pub guard: Option<Arc<CoreAst>>,
  /// Function scope (variables and passed arguments)
  pub scope: Arc<RwLock<Scope>>,
}

#[cfg(coreast)]
impl CoreFnClause {
  fn update_scope(scope: &RwLock<Scope>, ast: &CoreAst) {
    if let CoreAst::Var(v) = ast {
      if Scope::retrieve_var_from(scope, v).is_none() {
        Scope::add_to(scope, &v.name)
      }
    }
  }

  /// Creates a new Core Function Clause
  /// All input variables are given unique new names.
  pub fn new(clause_scope: Arc<RwLock<Scope>>,
             args_ast: &[Arc<CoreAst>],
             body: Arc<CoreAst>,
             guard: Option<Arc<CoreAst>>) -> Self {
    // let args: Vec<Arc<Var>> = args_ast.iter()
    //     .map(|arg_ast| ExtractVar::extract_vars(arg_ast))
    //     .flatten()
    //     .collect();
    // println!("New fnclause: args {:?}\nnew args {:?}", args_ast, args);
    args_ast.iter().for_each(|ast| Self::update_scope(&clause_scope, ast));

    // Create inner_env for each arg where it has any() type, later this can be amended
    // TOxDO: This creates new Arc<RwLock> for each clause argument, which is not slow but unnecessary
    // let inner_scope: Arc<RwLock<Scope>> = args_ast.iter()
    //     .fold(clause_scope.clone(), |scope, arg| {
    //       if let Ok(scope_r) = scope.read() {
    //         scope_r.add(arg).into_arc_rwlock()
    //       } else {
    //         panic!("Can't read-lock scope for creating clause scope while building core AST")
    //       }
    //     });

    Self {
      args_ast: args_ast.into(),
      body,
      guard,
      scope: clause_scope,
    }
  }
}

#[cfg(coreast)]
impl std::fmt::Display for CoreFnClause {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    Pretty::display_paren_list(&self.args_ast, f)?;
    match &self.guard {
      Some(g) => write!(f, " when {}", g)?,
      None => {}
    }
    write!(f, " -> {}", self.body)
  }
}
