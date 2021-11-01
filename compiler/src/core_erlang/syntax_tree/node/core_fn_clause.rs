//! Defines core function clause

use std::fmt::Formatter;
use std::sync::Arc;
use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::display;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_clause_type::FnClauseType;

/// Core function clause, keep it split without building a big case operator, for all function
/// clauses for type checking purposes.
#[derive(Debug)]
pub struct CoreFnClause {
  /// Argument match expressions directly translated from `ErlAst`
  pub args: Vec<Arc<CoreAst>>,
  /// Function clause body directly translated from `ErlAst`
  pub body: Arc<CoreAst>,
  /// The optional guard expression
  pub guard: Option<Arc<CoreAst>>,
}

impl std::fmt::Display for CoreFnClause {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    display::display_paren_list(&self.args, f)?;
    match &self.guard {
      Some(g) => write!(f, " when {}", g)?,
      None => {}
    }
    write!(f, " -> {}", self.body)
  }
}

impl CoreFnClause {
  /// Build `FnClauseType` from core function clause, together the clauses will form the full
  /// function type
  pub fn synthesize_clause_type(&self) -> FnClauseType {
    FnClauseType {
      args: self.args.iter()
          .map(|a| a.synthesize_type())
          .collect(),
      ret_ty: self.synthesize_clause_return_type(),
    }
  }

  /// Return type from the body AST
  pub fn synthesize_clause_return_type(&self) -> Arc<ErlType> { self.body.synthesize_type() }
}
