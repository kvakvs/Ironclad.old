//! Style of an expression, and style checker.

use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::erl_error::ErlError;
use crate::error::ic_error::IcResult;
use std::fmt::{Display, Formatter};

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

impl Display for ExprStyle {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      ExprStyle::Full => write!(f, "expression"),
      ExprStyle::Guard => write!(f, "guard expression"),
      ExprStyle::MatchExpr => write!(f, "match expression"),
      ExprStyle::Const => write!(f, "constant expression"),
    }
  }
}

impl AstNodeImpl {
  /// Returns `()` if the expression style matches, or returns a detailed error.
  pub fn verify_expr_style(node: &AstNode, style: ExprStyle) -> IcResult<()> {
    match &node.content {
      // AstNodeType::FnRef { .. } => {}
      AstNodeType::MFA { .. } => {
        if style == ExprStyle::Const {
          return ErlError::unacceptable(
            node.location.clone(),
            format!("References to functions are not allowed in {}", style),
          );
        }
      }
      AstNodeType::Var(_) => {
        if style == ExprStyle::Const {
          return ErlError::unacceptable(
            node.location.clone(),
            format!("Variables are not allowed in {}", style),
          );
        }
      }
      AstNodeType::Apply(_) => {
        if style == ExprStyle::Const {
          return ErlError::unacceptable(
            node.location.clone(),
            format!("Function applications are not allowed in {}", style),
          );
        }
      }
      AstNodeType::CaseExpr { .. } => {
        if style == ExprStyle::Const {
          return ErlError::unacceptable(
            node.location.clone(),
            format!("Case expressions are not allowed in {}", style),
          );
        }
      }
      AstNodeType::BinaryOp { binop_expr: expr } => {
        Self::verify_expr_style(&expr.left, style)?;
        Self::verify_expr_style(&expr.right, style)?;
      }
      AstNodeType::UnaryOp { unop_expr: expr } => {
        Self::verify_expr_style(&expr.expr, style)?;
      }
      AstNodeType::List { elements, tail } => {
        for e in elements.iter() {
          Self::verify_expr_style(e, style)?;
        }
        if let Some(t) = tail {
          Self::verify_expr_style(t, style)?;
        }
      }
      AstNodeType::Tuple { elements } => {
        for e in elements.iter() {
          Self::verify_expr_style(e, style)?;
        }
      }
      AstNodeType::MapBuilder { members } => {
        for m in members.iter() {
          Self::verify_expr_style(&m.key, style)?;
          Self::verify_expr_style(&m.expr, style)?;
        }
      }
      AstNodeType::RecordBuilder { base, members, .. } => {
        if let Some(b) = base {
          Self::verify_expr_style(b, style)?;
        }
        for m in members.iter() {
          Self::verify_expr_style(&m.expr, style)?;
        }
      }
      AstNodeType::RecordField { .. } => {
        if style == ExprStyle::Const {
          return ErlError::unacceptable(
            node.location.clone(),
            format!("Record fields are not allowed in {}", style),
          );
        }
      }
      // AstNodeType::CommaExpr { .. } => {}
      AstNodeType::ListComprehension { .. } => {
        if style == ExprStyle::Const {
          return ErlError::unacceptable(
            node.location.clone(),
            format!("List comprehensions are not allowed in {}", style),
          );
        }
      }
      AstNodeType::BinaryComprehension { .. } => {
        if style == ExprStyle::Const {
          return ErlError::unacceptable(
            node.location.clone(),
            format!("Binary comprehensions are not allowed in {}", style),
          );
        }
      }
      AstNodeType::ListComprehensionGenerator { .. } => {
        unreachable!("LC generators must not occur in the wild");
      }
      AstNodeType::CClause(_, _) => {
        unreachable!("Case clauses must not occur in the wild");
      }
      AstNodeType::FnDef(_) => {
        unreachable!("Function definitions must not occur in the wild");
      }
      AstNodeType::Type { .. } => {
        unreachable!("Types must not occur in the wild");
      }
      AstNodeType::TryCatch { .. } => {
        if style == ExprStyle::Const {
          return ErlError::unacceptable(
            node.location.clone(),
            format!("Try expressions are not allowed in {}", style),
          );
        }
      }
      AstNodeType::IfStatement { clauses } => {
        if style == ExprStyle::MatchExpr {
          return ErlError::unacceptable(
            node.location.clone(),
            format!("If expressions are not allowed in {}", style),
          );
        }
        // If is allowed even in constant and guard expressions, if evaluates to const
        for c in clauses.iter() {
          Self::verify_expr_style(&c.cond, style)?;
          Self::verify_expr_style(&c.body, style)?;
        }
      }
      // AstNodeType::BeginEnd { .. } => {}
      AstNodeType::BinaryExpr { elements } => {
        for e in elements.iter() {
          Self::verify_expr_style(&e.value, style)?;
        }
      }
      //-----------------------------------
      // No check necessary, always success
      //-----------------------------------
      AstNodeType::Lit { .. } | AstNodeType::Empty { .. } => {}
      // AstNodeType::ModuleForms { .. } => {}
      _ => {
        unimplemented!("AstNode::verify_expr_style: don't know how to verify {:?}", node.content)
      }
    }
    Ok(())
  }
}
