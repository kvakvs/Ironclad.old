//! Defines a case switch in Core Erlang

use crate::display;
use crate::syntaxtree::core_erl::core_ast::CoreAst;
use crate::typing::typevar::TypeVar;
use std::fmt::Formatter;

/// Case clause checks the input expression against `match_expr` and if it matches and if the guard
/// is true, the body will be executed.
pub struct CaseClause {
  /// The array of match expressions, one per case condition value
  pub match_exprs: Vec<CoreAst>,
  /// One unique type variable per match expression
  pub match_expr_types: Vec<TypeVar>,

  /// Guard condition, None if there's no condition (always true)
  pub guard: Option<CoreAst>,
  /// Unique type variable for guard condition expression
  pub guard_ty: Vec<TypeVar>,

  /// Clause body
  pub body: Box<CoreAst>,
  /// Case clause type
  pub ret_ty: TypeVar,
}

impl std::fmt::Display for CaseClause {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "( < ")?;
    display::display_comma_separated(&self.match_exprs, f)?;

    match &self.guard {
      None => write!(f, "> when 'true' -> ")?,
      Some(cond) => write!(f, "> when {} -> ", cond)?,
    };

    write!(f, "{} )", self.body)
  }
}

/// Case replaces Erlang constructs such as multiple function clauses (merged into one with a case),
/// `if` operator, `try of`, and `case of`.
pub struct Case {
  /// Case switch expressions, multiple are allowed
  pub exprs: Vec<CoreAst>,
  /// Case clauses in order. Each case must match every expression from `Self::exprs`
  pub clauses: Vec<CaseClause>,
  /// The unique typevar for return type, the union of clause return types
  pub ret_ty: TypeVar,
}

impl std::fmt::Display for Case {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self.exprs.len() {
      0 => unreachable!("Not allowed to have a `case` with 0 expressions"),
      1 => writeln!(f, "( case {} of ", self.exprs[0])?,
      _ => {
        writeln!(f, "( case <")?;
        display::display_comma_separated(&self.exprs, f)?;
        writeln!(f, " of ")?;
      },
    }
    for clause in &self.clauses {
      writeln!(f, "( {} )", clause)?;
    }
    write!(f, ")")
  }
}
