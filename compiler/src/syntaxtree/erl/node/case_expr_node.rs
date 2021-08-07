//! Defines CaseExpr struct for `case X of` AST node
use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::syntaxtree::erl::node::case_clause_node::CaseClauseNode;
use std::fmt::Formatter;
use crate::typing::typevar::TypeVar;

/// `Case X of ... end` expression AST node
// #[derive(PartialEq)]
pub struct CaseExprNode {
  /// A union type of all case clauses, also is the return type of the case expression
  pub ret_ty: TypeVar,
  /// Argument X in `case X of`
  pub arg: Box<ErlAst>,
  /// All case clauses in order
  pub clauses: Vec<CaseClauseNode>,
}

impl std::fmt::Display for CaseExprNode {
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
