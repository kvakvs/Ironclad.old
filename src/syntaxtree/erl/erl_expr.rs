use crate::syntaxtree::erl::erl_op::{ErlBinaryOp, ErlUnaryOp};
use crate::syntaxtree::erl::literal::ErlLiteral;

/// An Erlang expression
#[derive(Debug, Clone, PartialEq)]
pub enum ErlExpr {
  /// A named variable
  Var(String),
  /// A function call (Haskell/Lambda-calculus style with 1 argument), applying arg to target
  App { target: Box<ErlExpr>, arg: Box<ErlExpr> },

  /// A lambda definition or a function
  Function { args: Vec<ErlExpr>, expr: Box<ErlExpr> },

  /// A haskell-style new variable introducing a new scope below it:
  /// let x = expr1 in expr2
  Let { var: String, value: Box<ErlExpr>, in_expr: Box<ErlExpr> },

  /// A literal value, constant
  Lit(ErlLiteral),

  BinaryOp { left: Box<ErlExpr>, right: Box<ErlExpr>, op: ErlBinaryOp },
  UnaryOp { expr: Box<ErlExpr>, op: ErlUnaryOp },
  If { cond: Box<ErlExpr>, on_true: Box<ErlExpr>, on_false: Box<ErlExpr> },
}

impl ErlExpr {
  // pub fn lit_integer(value: isize) -> Self {
  //   ErlExpr::Lit(ErlLiteral::Integer(value))
  // }
}
