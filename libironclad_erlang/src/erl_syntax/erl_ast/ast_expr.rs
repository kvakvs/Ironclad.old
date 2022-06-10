//! Support for constant expressions (literals and expressions of literals) which can be calculated
//! in compile time.

use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::literal_bool::LiteralBool;
use crate::literal::Literal;

impl AstNodeImpl {
  /// Checks whether an expression is a `ErlAst::Lit`
  pub fn is_literal(&self) -> bool {
    matches!(self.content, AstNodeType::Lit { .. })
  }

  /// Walk the literal expression and try to find whether it is true, false or neither
  pub fn walk_boolean_litexpr(&self) -> LiteralBool {
    match &self.content {
      // ErlAst::CaseStatement { .. } => {}
      AstNodeType::Lit { .. } => {
        if !self.is_atom() {
          return LiteralBool::NotABoolean;
        }
        match self.as_atom() {
          "true" => LiteralBool::True,
          "false" => LiteralBool::False,
          _ => LiteralBool::NotABoolean,
        }
      }
      AstNodeType::BinaryOp { expr, .. } => expr.walk_boolean_litexpr(),
      AstNodeType::UnaryOp { expr, .. } => expr.walk_boolean_litexpr(),
      // TODO: Allow comma and semicolon expr for guards?
      // ErlAst::CommaExpr { .. } => {}
      // ErlAst::IfStatement { .. } => {}
      _ => LiteralBool::NotABoolean,
    }
  }

  /// Walk a literal expression and return `Some()` if its calculatable in compile time.
  pub fn walk_litexpr(&self) -> Option<Literal> {
    unimplemented!("Walk litexpr")
  }
}
