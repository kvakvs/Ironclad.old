//! Support for constant expressions (literals and expressions of literals) which can be calculated
//! in compile time.

use crate::syntax_tree::erl_ast::ErlAst;
use crate::syntax_tree::literal_bool::LiteralBool;

impl ErlAst {
  /// Checks whether an expression is a `ErlAst::Lit`
  pub fn is_literal(&self) -> bool {
    matches!(self, ErlAst::Lit { .. })
  }

  /// Walk the literal expression and try to find whether it is true, false or neither
  pub fn walk_boolean_litexpr(&self) -> LiteralBool {
    match self {
      // ErlAst::CaseStatement { .. } => {}
      ErlAst::Lit { .. } => {
        if !self.is_atom() {
          return LiteralBool::NotABoolean;
        }
        match self.as_atom() {
          "true" => LiteralBool::True,
          "false" => LiteralBool::False,
          _ => LiteralBool::NotABoolean,
        }
      }
      ErlAst::BinaryOp { expr, .. } => expr.walk_boolean_litexpr(),
      ErlAst::UnaryOp { expr, .. } => expr.walk_boolean_litexpr(),
      // TODO: Allow comma and semicolon expr for guards?
      // ErlAst::CommaExpr { .. } => {}
      // ErlAst::IfStatement { .. } => {}
      _ => LiteralBool::NotABoolean,
    }
  }
}
