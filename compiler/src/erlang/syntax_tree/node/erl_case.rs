//! Defines CaseExpr struct for `case X of` AST node
use std::fmt::Formatter;
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::node::erl_case_clause::ErlCaseClause;
use crate::typing::typevar::TypeVar;
use std::sync::Arc;

/// `Case X of ... end` expression AST node
// #[derive(PartialEq)]
pub struct ErlCase {
  /// A union type of all case clauses, also is the return type of the case expression
  pub ret_ty: TypeVar,
  /// Argument X in `case X of`
  pub arg: Arc<ErlAst>,
  /// All case clauses in order
  pub clauses: Vec<ErlCaseClause>,
}

impl std::fmt::Display for ErlCase {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "case {} of", self.arg)?;
    let mut first = true;
    for cc in self.clauses.iter() {
      if first { first = false; } else { writeln!(f, ";")?; }
      write!(f, "{}", cc)?;
    }
    write!(f, "end")
  }
}
