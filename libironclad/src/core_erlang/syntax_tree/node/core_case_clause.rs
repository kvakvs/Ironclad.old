//! Core Erlang Case operator clause
#![cfg(coreast)]
use std::fmt::Formatter;
use std::sync::Arc;

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use libironclad_util::pretty::Pretty;
use libironclad_util::source_loc::SourceLoc;

/// Case clause checks the input expression against `match_expr` and if it matches and if the guard
/// is true, the body will be executed.
#[derive(Debug)]
pub struct CaseClause {
  location: SourceLoc,
  /// The array of match expressions, one per case condition value
  pub(crate) match_exprs: Vec<Arc<CoreAst>>,

  /// Guard condition, None if there's no condition (always true)
  pub(crate) guard: Option<Arc<CoreAst>>,

  /// Clause body
  pub(crate) body: Arc<CoreAst>,
}

impl std::fmt::Display for CaseClause {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "<")?;
    Pretty::display_comma_separated(&self.match_exprs, f)?;

    match &self.guard {
      None => write!(f, "> -> ")?,
      Some(cond) => write!(f, "> when {} -> ", cond)?,
    };

    write!(f, "{}", self.body)
  }
}

impl CaseClause {
  /// Create a case clause, member of `Case`
  pub fn new(loc: SourceLoc, match_exprs: Vec<Arc<CoreAst>>, body: Arc<CoreAst>) -> CaseClause {
    CaseClause { location: loc, match_exprs, guard: None, body }
  }
}
