//! Defines a case switch in Core Erlang

use std::fmt::Formatter;
use std::sync::Arc;

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::core_erlang::syntax_tree::node::case_clause::CaseClause;
use crate::display;
use crate::source_loc::SourceLoc;
use crate::typing::typevar::TypeVar;

/// Case replaces Erlang constructs such as multiple function clauses (merged into one with a case),
/// `if` operator, `try of`, and `case of`.
pub struct Case {
  /// Source file pointer
  pub(crate) location: SourceLoc,
  /// Case switch expressions, multiple are allowed
  pub(crate) exprs: Vec<Arc<CoreAst>>,
  /// Case clauses in order. Each case must match every expression from `Self::exprs`
  pub(crate) clauses: Vec<CaseClause>,
  /// The unique typevar for return type, the union of clause return types
  pub(crate) ret_ty: TypeVar,
}

impl Case {
  /// Create a case struct, member of `CoreAst::Case`
  pub fn new(location: SourceLoc, exprs: Vec<Arc<CoreAst>>, clauses: Vec<CaseClause>) -> Case {
    Case {
      location,
      exprs,
      clauses,
      ret_ty: TypeVar::new()
    }
  }
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
