//! Contains iteration helpers for ErlAst

use crate::syntaxtree::erl::erl_ast::ErlAst;
use std::ops::Deref;
use std::cell::Ref;

/// Wraps either a simple reference or a runtime borrow from RefCell, for access to node children.
/// Lives as long as the parent who created this.
pub enum AstChild<'a> {
  /// Wraps a compile-time borrow via &
  Ref(&'a ErlAst),
  /// Wraps a runtime borrow via a RefCell
  RefCell(Ref<'a, ErlAst>),
}

impl ErlAst {
  // /// Mut iterator on the AST tree
  // /// For all children of the current node, apply the apply_fn to each child, allowing to
  // /// scan/recurse down the tree.
  // pub fn children_mut(&mut self) -> Option<Vec<&mut ErlAst>> {
  //   match self {
  //     ErlAst::ModuleAttr { .. } | ErlAst::Lit { .. } | ErlAst::Comment { .. }
  //     | ErlAst::Var { .. } => None,
  //
  //     ErlAst::ModuleForms(f) => Some(f.iter_mut().collect()),
  //
  //     ErlAst::FunctionDef { .. } => {
  //       // To descend into function defs, iterate over ErlModule.env.function_clauses
  //       None
  //     }
  //     // ErlAst::FunctionDef { index, .. } => {
  //     //   let fn_def = &env.functions[*index];
  //     //   let clauses = fn_def.get_clauses_mut(&mut env.function_clauses);
  //     //   let r: Vec<&mut ErlAst> = clauses.iter_mut()
  //     //       .map(|fclause| fclause.body.borrow_mut())
  //     //       .collect();
  //     //   Some(r)
  //     // }
  //
  //     ErlAst::App(_loc, app) => {
  //       let mut r: Vec<&mut ErlAst> = vec![&mut app.expr];
  //       r.extend(app.args.iter_mut());
  //       Some(r)
  //     }
  //
  //     ErlAst::Let(_loc, let_expr) => {
  //       Some(vec![&mut let_expr.value, &mut let_expr.in_expr])
  //     }
  //
  //     ErlAst::Case(_loc, case) => {
  //       let mut r: Vec<&mut ErlAst> = vec![case.arg.borrow_mut()];
  //       case.clauses.iter_mut().for_each(|cc| {
  //         r.push(&mut cc.cond);
  //         r.push(&mut cc.guard);
  //         r.push(&mut cc.body);
  //       });
  //       Some(r)
  //     }
  //
  //     ErlAst::CClause(_loc, clause) => {
  //       Some(vec![&mut clause.cond,
  //                 &mut clause.guard,
  //                 &mut clause.body])
  //     }
  //     ErlAst::BinaryOp(_loc, binop) => {
  //       Some(vec![&mut binop.left,
  //                 &mut binop.right])
  //     }
  //     ErlAst::UnaryOp(_loc, unop) => Some(vec![&mut unop.expr]),
  //     ErlAst::Comma { left, right, .. } => {
  //       Some(vec![left, right])
  //     }
  //     ErlAst::Token { .. } => panic!("Token {} must be eliminated in AST build phase", self),
  //     ErlAst::List(_loc, elems) => Some(elems.iter_mut().collect()),
  //     ErlAst::Tuple(_loc, elems) => Some(elems.iter_mut().collect()),
  //     _ => unreachable!("Can't process {}", self),
  //   }
  // }

  /// Const iterator on the AST tree
  /// For all children of the current node, apply the apply_fn to each child, allowing to
  /// scan/recurse down the tree.
  /// Returns `AstChild` wrapped because some nodes are RefCells.
  pub fn children(&self) -> Option<Vec<AstChild>> {
    match self {
      ErlAst::ModuleAttr { .. } | ErlAst::Lit { .. } | ErlAst::Comment { .. }
      | ErlAst::Var { .. } | ErlAst::FunArity(..) => None,

      ErlAst::ModuleForms(f) => Some(f.iter().map(AstChild::Ref).collect()),

      ErlAst::FunctionDef { fn_def, .. } => {
        let r: Vec<AstChild> = fn_def.clauses.iter()
            .map(|fclause| &*fclause.body)
            .map(AstChild::Ref)
            .collect();
        Some(r)
      }

      ErlAst::Apply(_loc, app) => {
        let expr = app.expr.deref();
        let mut r: Vec<AstChild> = vec![AstChild::RefCell(expr.borrow())];
        r.extend(app.args.iter().map(AstChild::Ref));
        Some(r)
      }

      ErlAst::Let(_loc, let_expr) => {
        Some(vec![AstChild::Ref(&let_expr.value),
                  AstChild::Ref(&let_expr.in_expr)])
      }

      ErlAst::Case(_loc, case) => {
        let mut r: Vec<AstChild> = vec![AstChild::Ref(&case.arg)];
        case.clauses.iter().for_each(|cc| {
          r.push(AstChild::Ref(&cc.cond));
          r.push(AstChild::Ref(&cc.guard));
          r.push(AstChild::Ref(&cc.body));
        });
        Some(r)
      }

      ErlAst::CClause(_loc, clause) => {
        Some(vec![AstChild::Ref(&clause.cond),
                  AstChild::Ref(&clause.guard),
                  AstChild::Ref(&clause.body)])
      }
      ErlAst::BinaryOp(_loc, binop) => {
        Some(vec![AstChild::Ref(&binop.left),
                  AstChild::Ref(&binop.right)])
      }
      ErlAst::UnaryOp(_loc, unop) => Some(vec![AstChild::Ref(&unop.expr)]),
      ErlAst::Comma { left, right, .. } => {
        Some(vec![AstChild::Ref(left),
                  AstChild::Ref(right)])
      }
      ErlAst::Token { .. } => panic!("Token {} must be eliminated in AST build phase", self),
      ErlAst::List(_loc, elems) => Some(elems.iter().map(AstChild::Ref).collect()),
      ErlAst::Tuple(_loc, elems) => Some(elems.iter().map(AstChild::Ref).collect()),

      _ => unreachable!("Can't process {}", self),
    }
  }
}
