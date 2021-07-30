//! Defines structs for AST nodes representing binary operators (A + B) and unary (+A)
use std::rc::Rc;

use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::syntaxtree::erl::erl_op::{ErlBinaryOp, ErlUnaryOp};
use crate::typing::erl_type::ErlType;

/// Binary operator is a code structure `Expr <operator> Expr`
#[derive(PartialEq)]
pub struct BinaryOperatorExprNode {
  /// Left operand
  pub left: Box<ErlAst>,
  /// Right operand
  pub right: Box<ErlAst>,
  /// The operator
  pub operator: ErlBinaryOp,
  /// The return type of the operation
  pub ty: ErlType,
}

/// Unary operator is right-associative operation such as `not A` or `+A`
#[derive(PartialEq)]
pub struct UnaryOperatorExprNode {
  /// The operand
  pub expr: Box<ErlAst>,
  /// The operator
  pub operator: ErlUnaryOp,
}
