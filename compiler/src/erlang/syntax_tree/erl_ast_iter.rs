//! Contains iteration helpers for ErlAst

use crate::erlang::syntax_tree::erl_ast::ErlAst;
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
  /// Const iterator on the AST tree
  /// For all children of the current node, apply the apply_fn to each child, allowing to
  /// scan/recurse down the tree.
  /// Returns `AstChild` wrapped because some nodes are RefCells.
  pub fn children(&self) -> Option<Vec<AstChild>> {
    match self {
      ErlAst::ModuleAttr { .. } | ErlAst::Lit { .. } | ErlAst::Comment { .. }
      | ErlAst::Var { .. } | ErlAst::MFA { .. } => None,

      ErlAst::ModuleForms(f) => Some(f.iter().map(AstChild::Ref).collect()),

      ErlAst::FnDef { fn_def, .. } => {
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
      // ErlAst::Comma { left, right, .. } => {
      //   Some(vec![AstChild::Ref(left),
      //             AstChild::Ref(right)])
      // }
      ErlAst::Token { .. } => panic!("Token {} must be eliminated in AST build phase", self),
      ErlAst::List { elements, .. } => Some(elements.iter().map(AstChild::Ref).collect()),
      ErlAst::Tuple { elements, .. } => Some(elements.iter().map(AstChild::Ref).collect()),

      _ => unreachable!("Can't process {}", self),
    }
  }
}
