//! Contains iteration helpers for ErlAst
use crate::erl_syntax::erl_ast::ErlAst;
use ::function_name::named;

use crate::erl_syntax::erl_ast::ErlAstType::{
  Apply, BinaryExpr, BinaryOp, CClause, CaseStatement, CommaExpr, ExportAttr, FnDef, FnSpec,
  IfStatement, ImportAttr, List, ListComprehension, ListComprehensionGenerator, Lit, ModuleForms,
  ModuleStartAttr, RecordDefinition, Token, TryCatch, Tuple, Type, UnaryOp, Var, MFA,
};
use crate::erl_syntax::node::erl_binary_element::ValueWidth;
use std::sync::Arc;

/// A trait for AST nodes which can contain nested nodes
pub trait AstNode {
  /// Return nested AST nodes for this node
  fn children(&self) -> Option<Vec<Arc<ErlAst>>>;
}

impl AstNode for ErlAst {
  /// Const iterator on the AST tree
  /// For all children of the current node, apply the apply_fn to each child, allowing to
  /// scan/recurse down the tree.
  /// Returns `AstChild` wrapped because some nodes are RefCells.
  #[named]
  fn children(&self) -> Option<Vec<Arc<ErlAst>>> {
    match &self.content {
      ModuleStartAttr { .. }
      | RecordDefinition { .. }
      | ExportAttr { .. }
      | ImportAttr { .. }
      | FnSpec { .. }
      | Lit { .. }
      | MFA { .. }
      | Type { .. }
      | Var { .. } => None,

      ModuleForms(f) => Some(f.to_vec()),
      FnDef(fn_def) => fn_def.children(),
      Apply(app) => app.children(),

      CaseStatement { expr, clauses, .. } => {
        let mut r: Vec<Arc<ErlAst>> = vec![expr.clone()];

        for cc in clauses {
          r.push(cc.pattern.clone());
          if let Some(g) = &cc.guard {
            r.push(g.clone());
          }
          r.push(cc.body.clone());
        }

        if r.is_empty() {
          None
        } else {
          Some(r)
        }
      }

      CClause(_loc, clause) => {
        if let Some(g) = &clause.guard {
          Some(vec![clause.pattern.clone(), g.clone(), clause.body.clone()])
        } else {
          Some(vec![clause.pattern.clone(), clause.body.clone()])
        }
      }
      BinaryOp { expr: binop_expr, .. } => {
        Some(vec![binop_expr.left.clone(), binop_expr.right.clone()])
      }
      UnaryOp { expr: unop_expr, .. } => Some(vec![unop_expr.expr.clone()]),
      List { elements, .. } => Some(elements.to_vec()),
      Tuple { elements, .. } => Some(elements.to_vec()),

      ListComprehension { expr, generators, .. } => {
        let mut result = vec![expr.clone()];
        result.extend(generators.iter().cloned());
        Some(result)
      }
      ListComprehensionGenerator { left, right, .. } => Some(vec![left.clone(), right.clone()]),

      Token { .. } => panic!("Token {} must be eliminated in AST build phase", self),
      TryCatch { body, of_branches, catch_clauses, .. } => {
        let mut r: Vec<Arc<ErlAst>> = body.children().unwrap_or_default();
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
        if r.is_empty() {
          None
        } else {
          Some(r)
        }
      }
      CommaExpr { elements, .. } => {
        let mut r = Vec::default();
        for e in elements.iter() {
          if let Some(c) = e.children() {
            r.extend(c.iter().cloned());
          }
        }
        if r.is_empty() {
          None
        } else {
          Some(r)
        }
      }
      IfStatement { clauses, .. } => {
        let mut r = Vec::default();
        for ifc in clauses.iter() {
          if let Some(c) = ifc.children() {
            r.extend(c.iter().cloned());
          }
        }
        if r.is_empty() {
          None
        } else {
          Some(r)
        }
      }
      BinaryExpr { elements, .. } => {
        let mut r = Vec::default();
        for bel in elements.iter() {
          if let ValueWidth::Expr(expr_width) = &bel.width {
            r.push(expr_width.clone());
          }
        }
        if r.is_empty() {
          None
        } else {
          Some(r)
        }
      }
      _ => unreachable!("{}(): Can't process {}", function_name!(), self),
    }
  }
}
