//! Defines Application AST node for a function call
use std::fmt::Formatter;
use std::sync::Arc;

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::display::display_comma_separated;
use crate::source_loc::SourceLoc;

/// AST node which contains a function call
pub struct ErlApply {
  /// Code location in the Erlang AST
  pub location: SourceLoc,
  /// Target, to be called, expected to have function or lambda type fun((arg, arg,...) -> ret)
  pub expr: Arc<ErlAst>,
  /// Function application arguments, list of expressions
  pub args: Vec<Arc<ErlAst>>,
}

impl std::fmt::Display for ErlApply {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}(", self.expr)?;
    display_comma_separated(&self.args, f)?;
    write!(f, ")")
  }
}

impl std::fmt::Debug for ErlApply {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "apply({:?}, (", self.expr)?;
    display_comma_separated(&self.args, f)?;
    write!(f, "))")
  }
}

impl ErlApply {
  // /// From argument types build a new ErlType::Function() with a single clause corresponding to
  // /// that specific call `Apply` would be performing
  // pub fn get_function_type(&self) -> ErlType {
  //   let arg_types: Vec<ErlType> = self.args.iter()
  //       .map(|arg| arg.get_type())
  //       .collect();
  //   let clause_type = FnClauseType::new(arg_types, self.ret_ty.into());
  //   let f_type = FunctionType::new(None, vec![clause_type]);
  //   ErlType::Fn(f_type)
  // }

  /// Creates a new function call (application) AST node
  pub fn new(location: SourceLoc, expr: Arc<ErlAst>, args: Vec<Arc<ErlAst>>) -> Self {
    ErlApply { location, expr, args }
  }

  // /// To use during the construction, from expression and arg expressions, assume that the
  // /// expression must be callable and build a `fun(Args...) -> Ret` type
  // fn create_expr_type(args: &[ErlAst], ret: &ErlType) -> ErlType {
  //   let arg_types = args.iter()
  //       .map(|a| a.get_type())
  //       .collect();
  //   let clause = FnClauseType::new(arg_types, ret.clone());
  //   // unnamed function application, None for a name
  //   ErlType::Fn(FunctionType::new(None, vec![clause]))
  // }

  // /// During post-parse scan try check if our expression is a reference to a known function.
  // /// If so, replace it with a pointer to that function.
  // pub fn postprocess_edit_node(&self, module: &Module) -> ErlResult<()> {
  //   // Check if expr (target) points to some existing function that we know
  //   let find_result = module.find_function_by_expr_arity(&self.expr.borrow(), self.args.len());
  //   match find_result {
  //     None => {} // no changes, return same node
  //     Some(index) => {
  //       let fn_def: &ErlFnDef = &module.functions[index];
  //
  //       // Replace self.expr with the found fun pointer
  //       let new_expr = ErlAst::MFA {
  //         location: SourceLoc::None,
  //         mfarity: fn_def.mfarity.clone(),
  //         clause_types: fn_def.clauses.iter().map(|fc| fc.get_type()).collect(),
  //         ret_ty: TypeVar::new(),
  //       };
  //
  //       println!("ApplicationNode: Replacing {} with {}", self.expr.borrow(), new_expr);
  //       self.expr.replace(new_expr);
  //     }
  //   }
  //   Ok(())
  // }
}