//! Defines structs for AST nodes representing ironclad_exe operators (A + B) and unary (+A)
use crate::syntax_tree::erl_ast::ErlAst;
use crate::syntax_tree::erl_op::ErlUnaryOp;
use libironclad_error::source_loc::SourceLoc;
use std::sync::Arc;

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
      expr: ErlUnaryOperatorExpr { expr, operator },
    }
    .into()
  }
}
