//! Contains iteration helpers for CoreAst
use function_name::named;
use std::sync::Arc;

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::core_erlang::syntax_tree::node::prim_op::PrimOp;

impl CoreAst {
  /// Const iterator on the Core AST tree
  /// For all children of the current node, apply the apply_fn to each child, allowing to
  /// scan/recurse down the tree.
  /// Returns `AstChild` wrapped because some nodes are RefCells.
  #[named]
  pub fn children(&self) -> Option<Vec<Arc<CoreAst>>> {
    match self {
      CoreAst::Attributes { .. } | CoreAst::Lit { .. } | CoreAst::Var { .. }
      | CoreAst::Module { .. } => None,

      CoreAst::FnDef(fn_def) => Some(vec![fn_def.body.clone()]),

      CoreAst::Apply(app) => {
        let mut r: Vec<Arc<CoreAst>> = vec![app.target.clone()];
        r.extend(app.args.iter().cloned());
        Some(r)
      }

      CoreAst::Case(case) => {
        let mut r: Vec<Arc<CoreAst>> = case.exprs.to_vec();

        for cc in case.clauses.iter() {
          if let Some(guard_cond) = &cc.guard {
            r.push(guard_cond.clone());
          }
          r.extend(cc.match_exprs.iter().cloned());
          r.push(cc.body.clone());
        }
        Some(r)
      }

      CoreAst::BinOp { op, .. } => {
        Some(vec![op.left.clone(),
                  op.right.clone()])
      }
      CoreAst::UnOp { op, .. } => Some(vec![op.expr.clone()]),
      CoreAst::List { elements, .. } => Some(elements.to_vec()),
      CoreAst::Tuple { elements, .. } => Some(elements.to_vec()),

      CoreAst::Empty => panic!("{}: Core AST tree is not initialized (empty node)", function_name!()),
      CoreAst::PrimOp { op, .. } => {
        match op {
          PrimOp::Raise { expr, .. } => Some(vec![expr.clone()]),
          PrimOp::ExcTrace => None,
        }
      }

      _ => {
        println!("{}: Can't process {}", function_name!(), self);
        unreachable!()
      },
    }
  }
}
