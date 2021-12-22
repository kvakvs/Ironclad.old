//! Defines structs for AST nodes representing binary operators (A + B) and unary (+A)
use std::sync::Arc;

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::erl_op::{ErlBinaryOp, ErlUnaryOp};
use crate::source_loc::SourceLoc;

/// Binary operator is a code structure `Expr <operator> Expr`
#[derive(Debug)]
pub struct ErlBinaryOperatorExpr {
  /// Left operand
  pub left: Arc<ErlAst>,
  /// Right operand
  pub right: Arc<ErlAst>,
  /// The operator
  pub operator: ErlBinaryOp,
}

/// Unary operator is right-associative operation such as `not A` or `+A`
#[derive(Debug)]
pub struct ErlUnaryOperatorExpr {
  /// The operand
  pub expr: Arc<ErlAst>,
  /// The operator
  pub operator: ErlUnaryOp,
}

impl ErlUnaryOperatorExpr {
  /// Create an unary operator and wrap it with ErlAst::UnaryOp
  pub fn new_ast(loc: SourceLoc, operator: ErlUnaryOp, expr: Arc<ErlAst>) -> Arc<ErlAst> {
    ErlAst::UnaryOp(
      loc,
      ErlUnaryOperatorExpr {
        expr,
        operator,
      }).into()
  }
}

impl ErlBinaryOperatorExpr {
  /// Create a binary operator, caller is to wrap it with ErlAst::BinOp(location, _)
  pub fn new(left: Arc<ErlAst>, op: ErlBinaryOp, right: Arc<ErlAst>) -> Self {
    Self {
      left,
      right,
      operator: op,
    }
  }

  /// From left and multiple right components, build a right-associative tree of expressions.
  /// Try pair last and one before last, then take result and pair with previous one, ... and so on
  pub fn new_right_assoc(loc: &SourceLoc, left: Arc<ErlAst>, tail: &[(ErlBinaryOp, Arc<ErlAst>)]) -> Arc<ErlAst> {
    if tail.is_empty() {
      return left;
    }

    // Take rightmost element in the tail[] array, together with the operator
    // And build the recursive tree from the remaining on the left
    let (op, right) = &tail[tail.len() - 1];
    let build_left_side = Self::new_right_assoc(loc, left, &tail[0..tail.len() - 1]);

    ErlAst::BinaryOp(
      loc.clone(),
      Self::new(build_left_side, *op, right.clone()),
    ).into()
  }

  /// From left and multiple right components, build a left-associative tree of expressions.
  /// Try pair first and the first element in tail, then take result and pair with second, ... and so on
  pub fn new_left_assoc(loc: &SourceLoc, left: Arc<ErlAst>, tail: &[(ErlBinaryOp, Arc<ErlAst>)]) -> Arc<ErlAst> {
    if tail.is_empty() {
      return left;
    }

    // Take leftmost element in the tail[] array, together with the operator
    // And build the recursive tree from the remaining on the left
    let (op, first) = &tail[0];
    let build_right_side = Self::new_left_assoc(loc, first.clone(), &tail[1..tail.len()]);

    ErlAst::BinaryOp(
      loc.clone(),
      Self::new(left, *op, build_right_side),
    ).into()
  }
}
