//! Defines Application AST node for a function call
use crate::erl_error::ErlResult;
use crate::erl_module::func_registry::FunctionRegistry;
use crate::source_loc::SourceLoc;
use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::typing::erl_type::ErlType;
use crate::typing::typevar::TypeVar;

/// AST node which contains a function call
pub struct ApplicationNode {
  /// Target, to be called, expected to have function or lambda type fun((arg, arg,...) -> ret)
  pub expr: Box<ErlAst>,
  /// Arguments. Their  inferred types are stored inside.
  pub args: Vec<ErlAst>,
  /// Inferred type of return. Always a new TypeVar().
  pub ret_ty: TypeVar,
  /// Inferred type of the expression, must be something callable
  pub expr_ty: TypeVar,
}

impl ApplicationNode {
  /// Creates a new function call (application) AST node
  pub fn new(expr: ErlAst, args: Vec<ErlAst>) -> Self {
    // let ret_ty = ErlType::new_typevar();
    // let expr_ty = Self::create_expr_type(&args, &ret_ty);
    ApplicationNode {
      expr: Box::new(expr),
      args,
      ret_ty: TypeVar::new(),
      expr_ty: TypeVar::new(),
    }
  }

  /// To use during the construction, from expression and arg expressions, assume that the
  /// expression must be callable and build a `fun(Args...) -> Ret` type
  fn create_expr_type(args: &[ErlAst], ret: &ErlType) -> ErlType {
    ErlType::new_fun_type(
      None, // unnamed function application
      args.iter()
          .map(|a| a.get_type())
          .collect(),
      ret.clone())
  }

  /// During post-parse scan try check if our expression is a reference to a known function.
  /// If so, replace it with a pointer to that function.
  pub fn postprocess_edit_node(&mut self, env: &mut FunctionRegistry) -> ErlResult<()> {
    println!("Postprocessing App()... {}({:?}) env {:?}", self.expr, self.args, env);

    // Check if expr (target) points to some existing function that we know
    match env.find_function_by_expr_arity(&self.expr, self.args.len()) {
      None => {} // no changes, return same node
      Some(index) => {
        let nf = &env.functions[index];

        // Replace self.expr with the found fun pointer
        let new_expr = ErlAst::FunArity(SourceLoc::default(),
                                        nf.funarity.clone());

        println!("ApplicationNode: Replacing {} with {}", self.expr, new_expr);
        self.expr = Box::new(new_expr);
      }
    }
    Ok(())
  }
}