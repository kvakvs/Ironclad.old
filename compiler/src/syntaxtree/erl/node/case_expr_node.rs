//! Defines CaseExpr struct for `case X of` AST node
use std::rc::Rc;

use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::typing::erl_type::ErlType;

/// `Case X of ... end` expression AST node
#[derive(PartialEq)]
pub struct CaseExprNode {
  /// A union type of all case clauses, also is the return type of the case expression
  pub ret: ErlType,
  /// Argument of the `case X of`
  pub arg: Rc<ErlAst>,
  /// All case clauses in order
  pub clauses: Vec<Rc<ErlAst>>, // TODO: turn into Vec<CClause>
}
