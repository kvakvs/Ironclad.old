//! Defines CaseExpr struct for `case X of` AST node
use std::fmt::Formatter;
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::node::case_clause::CaseClause;
use crate::typing::typevar::TypeVar;

/// `Case X of ... end` expression AST node
// #[derive(PartialEq)]
pub struct Case {
  /// A union type of all case clauses, also is the return type of the case expression
  pub ret_ty: TypeVar,
  /// Argument X in `case X of`
  pub arg: Box<ErlAst>,
  /// All case clauses in order
  pub clauses: Vec<CaseClause>,
}

impl std::fmt::Display for Case {
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
