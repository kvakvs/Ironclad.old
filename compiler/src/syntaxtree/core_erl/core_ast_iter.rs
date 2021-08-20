//! Contains iteration helpers for CoreAst

use function_name::named;
use std::ops::Deref;
use std::cell::Ref;
use crate::syntaxtree::core_erl::core_ast::CoreAst;

/// Wraps either a simple reference or a runtime borrow from RefCell, for access to node children.
/// Lives as long as the parent who created this.
pub enum AstChild<'a> {
  /// Wraps a compile-time borrow via &
  Ref(&'a CoreAst),
  /// Wraps a runtime borrow via a RefCell
  RefCell(Ref<'a, CoreAst>),
}

impl CoreAst {
  /// Const iterator on the Core AST tree
  /// For all children of the current node, apply the apply_fn to each child, allowing to
  /// scan/recurse down the tree.
  /// Returns `AstChild` wrapped because some nodes are RefCells.
  #[named]
  pub fn children(&self) -> Option<Vec<AstChild>> {
    match self {
      CoreAst::Attributes { .. } | CoreAst::Lit { .. } | CoreAst::Var { .. }
      | CoreAst::Module { .. } => None,

      CoreAst::FnDef { fn_def, .. } => {
        // let mut r: Vec<AstChild> = vec![AstChild::Ref(fn_def.body.deref())];
        // r.extend(fn_def.args.iter().map(AstChild::Ref));
        Some(vec![AstChild::Ref(&fn_def.body)])
      }

      CoreAst::Apply { app, .. } => {
        let mut r: Vec<AstChild> = vec![AstChild::Ref(app.target.deref())];
        r.extend(app.args.iter().map(AstChild::Ref));
        Some(r)
      }

      CoreAst::Case { case, .. } => {
        let mut r: Vec<AstChild> = Vec::new();
        r.extend(case.exprs.iter().map(|ce| AstChild::Ref(ce)));

        for cc in case.clauses.iter() {
          if let Some(guard_cond) = &cc.guard {
            r.push(AstChild::Ref(guard_cond));
          }
          r.extend(cc.match_exprs.iter().map(|me| AstChild::Ref(me)));
          r.push(AstChild::Ref(&cc.body));
        }
        Some(r)
      }

      CoreAst::BinOp { op, .. } => {
        Some(vec![AstChild::Ref(&op.left),
                  AstChild::Ref(&op.right)])
      }
      CoreAst::UnOp { op, .. } => Some(vec![AstChild::Ref(&op.expr)]),
      CoreAst::List { elements, .. } => Some(elements.iter().map(AstChild::Ref).collect()),
      CoreAst::Tuple { elements, .. } => Some(elements.iter().map(AstChild::Ref).collect()),

      _ => unreachable!("{}: Can't process {}", function_name!(), self),
    }
  }
}
