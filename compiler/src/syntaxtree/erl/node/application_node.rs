//! Defines Application AST node for a function call
use std::rc::Rc;

use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::syntaxtree::erl::node::t_postprocess::TPostProcess;
use crate::typing::erl_type::ErlType;
use crate::erl_module::ErlModule;

/// AST node which contains a function call
#[derive(Clone, PartialEq)]
pub struct ApplicationNode {
  /// Target, to be called, expected to have function or lambda type fun((arg, arg,...) -> ret)
  pub expr: Rc<ErlAst>,
  /// Arguments. Their  inferred types are stored inside.
  pub args: Vec<Rc<ErlAst>>,
  /// Inferred type of return. Always a new TypeVar().
  pub ret_type: ErlType,
  /// Inferred type of the expression, must be something callable
  pub expr_type: ErlType,
}

impl ApplicationNode {
  /// Creates a new function call (application) AST node
  pub fn new(expr: Rc<ErlAst>, args: Vec<Rc<ErlAst>>) -> Self {
    let ret_ty = ErlType::new_typevar();
    let expr_ty = Self::create_expr_type(&args, &ret_ty);
    ApplicationNode { expr, args, ret_type: ret_ty, expr_type: expr_ty }
  }

  /// To use during the construction, from expression and arg expressions, assume that the
  /// expression must be callable and build a `fun(Args...) -> Ret` type
  fn create_expr_type(args: &Vec<Rc<ErlAst>>, ret: &ErlType) -> ErlType {
    ErlType::new_fun_type(
      None, // unnamed function application
      args.iter()
          .map(|a| a.get_type())
          .collect(),
      ret.clone())
  }
}

impl TPostProcess for ApplicationNode {
  fn postprocess_ast(&self, env: &mut ErlModule, ast: &Rc<ErlAst>) -> Option<Rc<ErlAst>> {
    // Check if expr (target) points to some existing function that we know
    match env.find_function_expr_arity(ast, self.args.len()) {
      None => Some(ast.clone()), // no changes, return same node
      Some(found_fun) => {
        // TODO: replace self.expr with the found fun pointer
        let mut updated_app = self.clone();
        updated_app.expr = ErlAst::newfun_to_funarity(&found_fun).unwrap();
        Some(Rc::new(ErlAst::App(updated_app)))
      }
    }
  }
}