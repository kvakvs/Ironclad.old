//! Defines Application AST node for a function call
use std::cell::RefCell;

use crate::erl_error::ErlResult;
use crate::erl_module::ErlModule;
use crate::source_loc::SourceLoc;
use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::syntaxtree::erl::node::fn_def::FnDef;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_clause_type::FnClauseType;
use crate::typing::fn_type::FunctionType;
use crate::typing::typevar::TypeVar;
use std::fmt::Formatter;
use crate::display::display_comma_separated;

/// AST node which contains a function call
pub struct Apply {
  /// Target, to be called, expected to have function or lambda type fun((arg, arg,...) -> ret)
  pub expr: Box<RefCell<ErlAst>>,
  /// Arguments. Their inferred types are stored inside.
  pub args: Vec<ErlAst>,
  /// Inferred type of return. Always a new TypeVar().
  pub ret_ty: TypeVar,
  /// Inferred type of the expression, must be something callable
  pub expr_ty: TypeVar,
}

impl std::fmt::Display for Apply {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}(", self.expr.borrow())?;
    display_comma_separated(&self.args, f)?;
    write!(f, ")")
  }
}

impl Apply {
  /// From argument types build a new ErlType::Function() with a single clause corresponding to
  /// that specific call `Apply` would be performing
  pub fn get_function_type(&self) -> ErlType {
    let arg_types: Vec<ErlType> = self.args.iter()
        .map(|arg| arg.get_type())
        .collect();
    let clause_type = FnClauseType::new(arg_types, self.ret_ty.into());
    let f_type = FunctionType::new(None, vec![clause_type]);
    ErlType::Function(f_type)
  }

  /// Creates a new function call (application) AST node
  pub fn new(expr: ErlAst, args: Vec<ErlAst>) -> Self {
    // let ret_ty = ErlType::new_typevar();
    // let expr_ty = Self::create_expr_type(&args, &ret_ty);
    Apply {
      expr: Box::new(RefCell::new(expr)),
      args,
      ret_ty: TypeVar::new(),
      expr_ty: TypeVar::new(),
    }
  }

  /// To use during the construction, from expression and arg expressions, assume that the
  /// expression must be callable and build a `fun(Args...) -> Ret` type
  fn create_expr_type(args: &[ErlAst], ret: &ErlType) -> ErlType {
    let arg_types = args.iter()
        .map(|a| a.get_type())
        .collect();
    let clause  = FnClauseType::new(arg_types, ret.clone());
    // unnamed function application, None for a name
    ErlType::Function(FunctionType::new(None, vec![clause]))
  }

  /// During post-parse scan try check if our expression is a reference to a known function.
  /// If so, replace it with a pointer to that function.
  pub fn postprocess_edit_node(&self, module: &ErlModule) -> ErlResult<()> {
    println!("Postprocessing App()... {}", self);

    // Check if expr (target) points to some existing function that we know
    let find_result = module.find_function_by_expr_arity(&self.expr.borrow(), self.args.len());
    match find_result {
      None => {} // no changes, return same node
      Some(index) => {
        let nf: &FnDef = &module.functions[index];

        // Replace self.expr with the found fun pointer
        let new_expr = ErlAst::FunArity(SourceLoc::default(),
                                        nf.funarity.clone());

        println!("ApplicationNode: Replacing {} with {}", self.expr.borrow(), new_expr);
        self.expr.replace(new_expr);
      }
    }
    Ok(())
  }
}