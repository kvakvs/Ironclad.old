//! Contains iteration helpers for ErlAst
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use ::function_name::named;

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
    match self {
      ErlAst::ModuleStartAttr { .. }
      | ErlAst::ExportAttr { .. }
      | ErlAst::ImportAttr { .. }
      | ErlAst::FnSpec { .. }
      | ErlAst::Lit { .. }
      | ErlAst::MFA { .. }
      | ErlAst::Type { .. }
      | ErlAst::Var { .. } => None,

      ErlAst::ModuleForms(f) => Some(f.to_vec()),
      ErlAst::FnDef(fn_def) => fn_def.children(),
      ErlAst::Apply(app) => app.children(),

      ErlAst::Case(_loc, case) => {
        let mut r: Vec<Arc<ErlAst>> = vec![case.arg.clone()];
        case.clauses.iter().for_each(|cc| {
          r.push(cc.pattern.clone());
          if let Some(g) = &cc.guard {
            r.push(g.clone());
          }
          r.push(cc.body.clone());
        });
        if r.is_empty() { None } else { Some(r) }
      }

      ErlAst::CClause(_loc, clause) => {
        if let Some(g) = &clause.guard {
          Some(vec![clause.pattern.clone(),
                    g.clone(),
                    clause.body.clone()])
        } else {
          Some(vec![clause.pattern.clone(),
                    clause.body.clone()])
        }
      }
      ErlAst::BinaryOp { expr: binop_expr, .. } => {
        Some(vec![binop_expr.left.clone(),
                  binop_expr.right.clone()])
      }
      ErlAst::UnaryOp { expr: unop_expr, .. } => Some(vec![unop_expr.expr.clone()]),
      ErlAst::List { elements, .. } => Some(elements.to_vec()),
      ErlAst::Tuple { elements, .. } => Some(elements.to_vec()),

      ErlAst::ListComprehension { expr, generators, .. } => {
        let mut result = vec![expr.clone()];
        result.extend(generators.iter().cloned());
        Some(result)
      }
      ErlAst::ListComprehensionGenerator { left, right, .. } => {
        Some(vec![left.clone(), right.clone()])
      }

      ErlAst::Token { .. } => panic!("Token {} must be eliminated in AST build phase", self),
      ErlAst::TryCatch { body, of_branches, catch_clauses, .. } => {
        let mut r: Vec<Arc<ErlAst>> = body.children().unwrap_or_else(Vec::default);
        // For all of-branches, extend the result with each branch
        if let Some(ofb) = of_branches {
          for cc in ofb {
            if let Some(cc_children) = cc.children() {
              r.extend(cc_children.iter().cloned())
            }
          }
        }
        for cc in catch_clauses {
          if let Some(cc_children) = cc.children() {
            r.extend(cc_children.iter().cloned())
          }
        }
        if r.is_empty() { None } else { Some(r) }
      }
      ErlAst::CommaExpr {elements, ..} => {
        let mut r = Vec::default();
        elements.iter().for_each(|e| {
          if let Some(c) = e.children() {
            r.extend(c.iter().cloned());
          }
        });
        if r.is_empty() { None } else { Some(r) }
      }
      ErlAst::IfStatement {clauses, ..} => {
        let mut r = Vec::default();
        clauses.iter().for_each(|ifc| {
          if let Some(c) = ifc.children() {
            r.extend(c.iter().cloned());
          }
        });
        if r.is_empty() { None } else { Some(r) }
      }
      _ => unreachable!("{}(): Can't process {}", function_name!(), self),
    }
  }
}
