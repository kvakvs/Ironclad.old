//! Defines structs for AST nodes representing ironclad_exe operators (A + B) and unary (+A)
use crate::syntax_tree::erl_ast::ErlAst;
use crate::syntax_tree::erl_ast::ErlAstType::UnaryOp;
use crate::syntax_tree::erl_op::ErlUnaryOp;
use crate::syntax_tree::literal_bool::LiteralBool;
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
  pub fn new_ast(loc: &SourceLoc, operator: ErlUnaryOp, expr: Arc<ErlAst>) -> Arc<ErlAst> {
    let unop_node = UnaryOp { expr: ErlUnaryOperatorExpr { expr, operator } };
    ErlAst::construct_with_location(loc.clone(), unop_node)
  }

  /// Walk the literal expression and try to find whether it is true, false or neither
  pub fn walk_boolean_litexpr(&self) -> LiteralBool {
    match self.operator {
      ErlUnaryOp::Not => self.expr.walk_boolean_litexpr().negate(),
      _ => LiteralBool::NotABoolean,
    }
  }
}
