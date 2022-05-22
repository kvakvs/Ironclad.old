//! Defines a case switch in Core Erlang
#![cfg(coreast)]

use std::fmt::Formatter;
use std::sync::Arc;

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::core_erlang::syntax_tree::node::core_case_clause::CaseClause;
use libironclad_util::pretty::Pretty;
use libironclad_util::source_loc::SourceLoc;

/// Case replaces Erlang constructs such as multiple function clauses (merged into one with a case),
/// `if` operator, `try of`, and `case of`.
#[derive(Debug)]
#[cfg(coreast)]
pub struct Case {
  /// Source file pointer
  pub location: SourceLoc,
  /// Case switch expressions, multiple are allowed
  pub exprs: Vec<Arc<CoreAst>>,
  /// Case clauses in order. Each case must match every expression from `Self::exprs`
  pub clauses: Vec<CaseClause>,
}

#[cfg(coreast)]
impl Case {
  /// Create a case struct, member of `CoreAst::Case`
  pub fn new(location: SourceLoc, exprs: Vec<Arc<CoreAst>>, clauses: Vec<CaseClause>) -> Case {
    Case {
      location,
      exprs,
      clauses,
    }
  }
}

#[cfg(coreast)]
impl std::fmt::Display for Case {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self.exprs.len() {
      0 => writeln!(f, "( case <error empty exprs> of ")?,
      // 1 => writeln!(f, "( case <{}> of ", self.exprs[0])?,
      _ => {
        write!(f, "( case <")?;
        Pretty::display_comma_separated(&self.exprs, f)?;
        writeln!(f, "> of ")?;
      },
    }
    for clause in &self.clauses {
      writeln!(f, "( {} )", clause)?;
    }
    write!(f, ")")
  }
}
