//! Contains iteration helpers for ErlAst
use crate::erlang::syntax_tree::erl_ast::ErlAst;

use std::sync::Arc;

impl ErlAst {
  /// Const iterator on the AST tree
  /// For all children of the current node, apply the apply_fn to each child, allowing to
  /// scan/recurse down the tree.
  /// Returns `AstChild` wrapped because some nodes are RefCells.
  pub fn children(&self) -> Option<Vec<Arc<ErlAst>>> {
    match self {
      ErlAst::ModuleAttr { .. } | ErlAst::Lit { .. } | ErlAst::Comment { .. }
      | ErlAst::Var { .. } | ErlAst::MFA { .. } => None,

      ErlAst::ModuleForms(f) => Some(f.iter().cloned().collect()),

      ErlAst::FnDef(fn_def) => {
        let r: Vec<Arc<ErlAst>> = fn_def.clauses.iter()
            .map(|fclause| fclause.body.clone())
            .collect();
        Some(r)
      }

      ErlAst::Apply(app) => {
        let mut r: Vec<Arc<ErlAst>> = vec![app.expr.clone()];
        r.extend(app.args.iter().cloned());
        Some(r)
      }

      ErlAst::Case(_loc, case) => {
        let mut r: Vec<Arc<ErlAst>> = vec![case.arg.clone()];
        case.clauses.iter().for_each(|cc| {
          r.push(cc.cond.clone());
          r.push(cc.guard.clone());
          r.push(cc.body.clone());
        });
        Some(r)
      }

      ErlAst::CClause(_loc, clause) => {
        Some(vec![clause.cond.clone(),
                  clause.guard.clone(),
                  clause.body.clone()])
      }
      ErlAst::BinaryOp(_loc, binop) => {
        Some(vec![binop.left.clone(),
                  binop.right.clone()])
      }
      ErlAst::UnaryOp(_loc, unop) => Some(vec![unop.expr.clone()]),
      ErlAst::List { elements, .. } => Some(elements.iter().cloned().collect()),
      ErlAst::Tuple { elements, .. } => Some(elements.iter().cloned().collect()),

      ErlAst::Token { .. } => panic!("Token {} must be eliminated in AST build phase", self),
      _ => unreachable!("Can't process {}", self),
    }
  }
}
