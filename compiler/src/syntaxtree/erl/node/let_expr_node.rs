//! Haskell-style `let x = Value in Expr` node
use std::rc::Rc;

use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::typing::erl_type::ErlType;

/// Represents Haskell-style `let x = Value in Expr` node, might be useful in Erlang too?
#[derive(PartialEq)]
pub struct LetExprNode {
  /// The variable name assigned in let..in
  pub var: String,
  /// Type which we believe the Variable will have
  pub var_ty: ErlType,
  /// Value (type is in it)
  pub value: Rc<ErlAst>,
  /// Let x=y in <body> (type is in it, and becomes type of Expr::Let)
  pub in_expr: Rc<ErlAst>,
  /// The let .. in ... result type
  pub in_ty: ErlType,
}
