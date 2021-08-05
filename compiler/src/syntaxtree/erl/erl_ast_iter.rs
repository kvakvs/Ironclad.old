//! Contains iteration helpers for ErlAst

use crate::syntaxtree::erl::erl_ast::ErlAst;
use std::borrow::{BorrowMut, Borrow};

impl ErlAst {
  /// Mut iterator on the AST tree
  /// For all children of the current node, apply the apply_fn to each child, allowing to
  /// scan/recurse down the tree.
  pub fn children_mut(&mut self) -> Option<Vec<&mut ErlAst>> {
    match self {
      ErlAst::ModuleAttr { .. } | ErlAst::Lit { .. } | ErlAst::Comment { .. }
      | ErlAst::Var { .. } => None,

      ErlAst::ModuleForms(f) => Some(f.iter_mut().collect()),

      ErlAst::NewFunction(_loc, nf) => {
        let r: Vec<&mut ErlAst> = nf.clauses.iter_mut()
            .map(|fclause| fclause.body.borrow_mut())
            .collect();
        Some(r)
      }

      // ErlAst::FClause(_loc, fc) => {
      //   // Descend into args, and the body
      //   let mut r: Vec<&mut ErlAst> = fc.args.iter_mut().collect();
      //   r.push(&mut fc.body)?;
      //   Some(r)
      // }

      ErlAst::App(_loc, app) => {
        let mut r: Vec<&mut ErlAst> = vec![&mut app.expr];
        r.extend(app.args.iter_mut());
        Some(r)
      }

      ErlAst::Let(_loc, let_expr) => {
        Some(vec![&mut let_expr.value, &mut let_expr.in_expr])
      }

      ErlAst::Case(_loc, case) => {
        let mut r: Vec<&mut ErlAst> = vec![case.arg.borrow_mut()];
        case.clauses.iter_mut().for_each(|cc| {
          r.push(&mut cc.cond);
          r.push(&mut cc.guard);
          r.push(&mut cc.body);
        });
        Some(r)
      }

      ErlAst::CClause(_loc, clause) => {
        Some(vec![&mut clause.cond,
                  &mut clause.guard,
                  &mut clause.body])
      }
      ErlAst::BinaryOp(_loc, binop) => {
        Some(vec![&mut binop.left,
                  &mut binop.right])
      }
      ErlAst::UnaryOp(_loc, unop) => Some(vec![&mut unop.expr]),
      ErlAst::Comma { left, right, .. } => {
        Some(vec![left, right])
      }
      ErlAst::Token { .. } => panic!("Token {} must be eliminated in AST build phase", self),

      _ => unreachable!("Can't process {}", self),
    }
  }

  /// Const iterator on the AST tree
  /// For all children of the current node, apply the apply_fn to each child, allowing to
  /// scan/recurse down the tree.
  pub fn children(&self) -> Option<Vec<&ErlAst>> {
    match self {
      ErlAst::ModuleAttr { .. } | ErlAst::Lit { .. } | ErlAst::Comment { .. }
      | ErlAst::Var { .. } => None,

      ErlAst::ModuleForms(f) => Some(f.iter().collect()),

      ErlAst::NewFunction(_loc, nf) => {
        let r: Vec<&ErlAst> = nf.clauses.iter()
            .map(|fclause| fclause.body.borrow())
            .collect();
        Some(r)
      }

      ErlAst::App(_loc, app) => {
        let mut r: Vec<&ErlAst> = vec![&app.expr];
        r.extend(app.args.iter());
        Some(r)
      }

      ErlAst::Let(_loc, let_expr) => {
        Some(vec![&let_expr.value, &let_expr.in_expr])
      }

      ErlAst::Case(_loc, case) => {
        let mut r: Vec<&ErlAst> = vec![&case.arg];
        case.clauses.iter().for_each(|cc| {
          r.push(&cc.cond);
          r.push(&cc.guard);
          r.push(&cc.body);
        });
        Some(r)
      }

      ErlAst::CClause(_loc, clause) => {
        Some(vec![&clause.cond,
                  &clause.guard,
                  &clause.body])
      }
      ErlAst::BinaryOp(_loc, binop) => {
        Some(vec![&binop.left,
                  &binop.right])
      }
      ErlAst::UnaryOp(_loc, unop) => Some(vec![&unop.expr]),
      ErlAst::Comma { left, right, .. } => {
        Some(vec![left, right])
      }
      ErlAst::Token { .. } => panic!("Token {} must be eliminated in AST build phase", self),

      _ => unreachable!("Can't process {}", self),
    }
  }
}
