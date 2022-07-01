//! Style of an expression, and style checker.

use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::erl_ast::AstNode;
use crate::error::ic_error::IcResult;

/// Controls the parser behaviour and allowed operators.
#[derive(Eq, PartialEq, Copy, Clone)]
pub enum ExprStyle {
  /// Full expression including comma operator, for function bodies
  Full,
  /// Full expression, also using comma and semicolon as boolean combinators: used in guard exprs
  Guard,
  /// Match expression: comma, semicolon not allowed, function calls not allowed, etc...
  MatchExpr,
  /// Do not allow dynamic expression components: lambdas, function calls, variables etc
  Const,
}

impl AstNodeImpl {
  /// Returns `()` if the expression style matches, or returns a detailed error.
  pub fn verify_expr_style(node: &AstNode, style: ExprStyle) -> IcResult<()> {
    match &node.content {
      // AstNodeType::Empty { .. } => {}
      // AstNodeType::ModuleForms { .. } => {}
      // AstNodeType::FnDef(_) => {}
      // AstNodeType::FnRef { .. } => {}
      // AstNodeType::Type { .. } => {}
      // AstNodeType::CClause(_, _) => {}
      // AstNodeType::MFA { .. } => {}
      // AstNodeType::Var(_) => {}
      // AstNodeType::Apply(_) => {}
      // AstNodeType::CaseExpr { .. } => {}
      // AstNodeType::Lit { .. } => {}
      // AstNodeType::BinaryOp { .. } => {}
      // AstNodeType::UnaryOp { .. } => {}
      // AstNodeType::List { .. } => {}
      // AstNodeType::Tuple { .. } => {}
      // AstNodeType::MapBuilder { .. } => {}
      // AstNodeType::RecordBuilder { .. } => {}
      // AstNodeType::RecordField { .. } => {}
      // AstNodeType::CommaExpr { .. } => {}
      // AstNodeType::ListComprehension { .. } => {}
      // AstNodeType::BinaryComprehension { .. } => {}
      // AstNodeType::ListComprehensionGenerator { .. } => {}
      // AstNodeType::TryCatch { .. } => {}
      // AstNodeType::IfStatement { .. } => {}
      // AstNodeType::BeginEnd { .. } => {}
      // AstNodeType::BinaryExpr { .. } => {}
      _ => {
        unimplemented!("AstNode::verify_expr_style: don't know how to handle {:?}", node.content)
      }
    }
    Ok(())
  }
}
