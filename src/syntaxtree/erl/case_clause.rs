use crate::syntaxtree::erl::erl_ast::ErlAst;

#[derive(Debug, Clone, PartialEq)]
pub struct CaseClause {
  /// A match expression, matched vs. case arg
  cond: ErlAst,
  /// Must resolve to bool, or an exception
  guard: ErlAst,
  body: ErlAst,
}
