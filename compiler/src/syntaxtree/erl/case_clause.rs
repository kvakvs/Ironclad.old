use std::rc::Rc;

use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::typing::erl_type::ErlType;

#[derive(PartialEq)]
pub struct CaseClause {
  /// A match expression, matched vs. case arg
  pub cond: Rc<ErlAst>,
  /// Must resolve to bool, or an exception
  pub guard: Rc<ErlAst>,
  /// Case clause body expression
  pub body: Rc<ErlAst>,
  /// Clause body type, for type inference
  pub ty: ErlType,
}
