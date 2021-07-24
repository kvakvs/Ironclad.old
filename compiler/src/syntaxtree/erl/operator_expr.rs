//! Defines structs for AST nodes representing binary operators (A + B) and unary (+A)
use std::rc::Rc;

use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::syntaxtree::erl::erl_op::{ErlBinaryOp, ErlUnaryOp};
use crate::typing::erl_type::ErlType;

/// Binary operator is a code structure `Expr <operator> Expr`
#[derive(PartialEq)]
pub struct BinaryOperatorExpr {
  /// Left operand
  pub left: Rc<ErlAst>,
  /// Right operand
  pub right: Rc<ErlAst>,
  /// The operator
  pub operator: ErlBinaryOp,
  /// The return type of the operation
  pub ty: ErlType,
}

/// Unary operator is right-associative operation such as `not A` or `+A`
#[derive(PartialEq)]
pub struct UnaryOperatorExpr {
  /// The operand
  pub expr: Rc<ErlAst>,
  /// The operator
  pub operator: ErlUnaryOp,
}
