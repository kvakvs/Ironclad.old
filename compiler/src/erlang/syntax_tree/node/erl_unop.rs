//! Defines structs for AST nodes representing binary operators (A + B) and unary (+A)
use std::sync::Arc;

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::erl_op::{ErlUnaryOp};
use crate::source_loc::SourceLoc;

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
    ErlAst::UnaryOp {
      location: loc,
      expr: ErlUnaryOperatorExpr {
        expr,
        operator,
      },
    }.into()
  }
}
