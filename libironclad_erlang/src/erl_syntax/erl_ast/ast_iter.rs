//! Contains iteration helpers for ErlAst
use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_binary_element::ValueWidth;

/// A trait for AST nodes which can contain nested nodes
pub trait IterableAstNodeT {
  /// Return nested AST nodes for this node
  fn children(&self) -> Option<Vec<AstNode>>;
}

#[inline]
fn return_some_vec(vec: Vec<AstNode>) -> Option<Vec<AstNode>> {
  if vec.is_empty() {
    None
  } else {
    Some(vec)
  }
}

impl IterableAstNodeT for AstNodeImpl {
  /// Const iterator on the AST tree
  /// For all children of the current node, apply the apply_fn to each child, allowing to
  /// scan/recurse down the tree.
  /// Returns `AstChild` wrapped because some nodes are RefCells.
  fn children(&self) -> Option<Vec<AstNode>> {
    match &self.content {
      AstNodeType::Empty { .. }
      | AstNodeType::Lit { .. }
      | AstNodeType::MFA { .. }
      | AstNodeType::Type { .. }
      | AstNodeType::FnRef { .. }
      | AstNodeType::Var { .. } => None,

      AstNodeType::ModuleForms { forms: f, .. } => Some(f.to_vec()),
      AstNodeType::FnDef(fn_def) => fn_def.children(),
      AstNodeType::Apply(app) => app.children(),

      AstNodeType::CaseExpr { expr, clauses, .. } => {
        let mut r: Vec<AstNode> = vec![expr.clone()];

        for cc in clauses {
          r.push(cc.pattern.clone());
          if let Some(g) = &cc.guard {
            r.push(g.clone());
          }
          r.push(cc.body.clone());
        }
        return_some_vec(r)
      }

      AstNodeType::CClause(_loc, clause) => {
        if let Some(g) = &clause.guard {
          Some(vec![clause.pattern.clone(), g.clone(), clause.body.clone()])
        } else {
          Some(vec![clause.pattern.clone(), clause.body.clone()])
        }
      }
      AstNodeType::BinaryOp { binop_expr, .. } => {
        Some(vec![binop_expr.left.clone(), binop_expr.right.clone()])
      }
      AstNodeType::UnaryOp { unop_expr, .. } => Some(vec![unop_expr.expr.clone()]),
      AstNodeType::List { elements, .. } => Some(elements.to_vec()),
      AstNodeType::Tuple { elements, .. } => Some(elements.to_vec()),

      AstNodeType::ListComprehension { expr, generators, .. } => {
        let mut result = vec![expr.clone()];
        result.extend(generators.iter().cloned());
        Some(result)
      }
      AstNodeType::BinaryComprehension { expr, generators, .. } => {
        let mut result = vec![expr.clone()];
        result.extend(generators.iter().cloned());
        Some(result)
      }
      AstNodeType::ListComprehensionGenerator { left, right, .. } => {
        Some(vec![left.clone(), right.clone()])
      }

      AstNodeType::TryCatch { body, of_branches, catch_clauses, .. } => {
        let mut r: Vec<AstNode> = body.children().unwrap_or_default();
        // For all of-branches, extend the result with each branch
        if let Some(ofb) = of_branches {
          for cc in ofb {
            if let Some(cc_children) = cc.children() {
              r.extend(cc_children.iter().cloned())
            }
          }
        }
        for cc in catch_clauses.iter() {
          if let Some(cc_children) = cc.children() {
            r.extend(cc_children.iter().cloned())
          }
        }
        return_some_vec(r)
      }
      AstNodeType::CommaExpr { elements, .. } => {
        let mut r = Vec::default();
        for e in elements.iter() {
          if let Some(c) = e.children() {
            r.extend(c.iter().cloned());
          }
        }
        return_some_vec(r)
      }
      AstNodeType::IfStatement { clauses, .. } => {
        let mut r = Vec::default();
        for ifc in clauses.iter() {
          if let Some(c) = ifc.children() {
            r.extend(c.iter().cloned());
          }
        }
        return_some_vec(r)
      }
      AstNodeType::BeginEnd { exprs } => {
        let mut r = Vec::default();
        for e in exprs.iter() {
          if let Some(c) = e.children() {
            r.extend(c.iter().cloned());
          }
        }
        return_some_vec(r)
      }
      AstNodeType::BinaryExpr { elements, .. } => {
        let mut r = Vec::default();
        for bel in elements.iter() {
          if let ValueWidth::Expr(expr_width) = &bel.width {
            r.push(expr_width.clone());
          }
        }
        return_some_vec(r)
      }
      AstNodeType::RecordBuilder { members, .. } => {
        let mut r = Vec::default();
        for m in members {
          r.push(m.expr.clone());
        }
        return_some_vec(r)
      }
      AstNodeType::MapBuilder { members } => {
        let mut r = Vec::default();
        for m in members {
          if let Some(c) = m.expr.children() {
            r.extend(c.iter().cloned());
          }
        }
        return_some_vec(r)
      }
      // _ => {
      //   unimplemented!("{}:{}(): Can't process {:?}", file!(), function_name!(), self.content)
      // }
      AstNodeType::RecordField { base, .. } => Some(vec![base.clone()]),
    }
  }
}
