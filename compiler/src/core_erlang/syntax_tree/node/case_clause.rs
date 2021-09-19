//! Core Erlang Case operator clause
use std::fmt::Formatter;
use std::sync::Arc;

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::display;
use crate::source_loc::SourceLoc;
use crate::typing::typevar::TypeVar;

/// Case clause checks the input expression against `match_expr` and if it matches and if the guard
/// is true, the body will be executed.
#[derive(Debug)]
pub struct CaseClause {
  location: SourceLoc,
  /// The array of match expressions, one per case condition value
  pub(crate) match_exprs: Vec<Arc<CoreAst>>,
  /// One unique type variable per match expression
  match_expr_types: Vec<TypeVar>,

  /// Guard condition, None if there's no condition (always true)
  pub(crate) guard: Option<Arc<CoreAst>>,
  /// Unique type variable for guard condition expression
  guard_ty: TypeVar,

  /// Clause body
  pub(crate) body: Arc<CoreAst>,
  /// Case clause type
  pub(crate) ret_ty: TypeVar,
}

impl std::fmt::Display for CaseClause {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "<")?;
    display::display_comma_separated(&self.match_exprs, f)?;

    match &self.guard {
      None => write!(f, "> -> ")?,
      Some(cond) => write!(f, "> when {} -> ", cond)?,
    };

    write!(f, "{}", self.body)
  }
}

impl CaseClause {
  /// Create a case clause, member of `Case`
  pub fn new(loc: SourceLoc, match_exprs: Vec<Arc<CoreAst>>, match_expr_types: Vec<TypeVar>,
             body: Arc<CoreAst>) -> CaseClause {
    CaseClause {
      location: loc,
      match_exprs,
      match_expr_types,
      guard: None,
      guard_ty: TypeVar::new(),
      body,
      ret_ty: TypeVar::new(),
    }
  }
}