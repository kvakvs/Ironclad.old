//! Defines Application AST node for a function call
use std::fmt::Formatter;
use std::ops::Deref;
use std::sync::{Arc, RwLock};
use crate::display::Pretty;
use crate::erl_error::{ErlResult};

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::source_loc::SourceLoc;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_type::FnType;
use crate::typing::scope::Scope;
use crate::typing::type_error::TypeError;

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
    Pretty::display_comma_separated(&self.args, f)?;
    write!(f, ")")
  }
}

impl std::fmt::Debug for ErlApply {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "apply({:?}, (", self.expr)?;
    Pretty::display_comma_separated(&self.args, f)?;
    write!(f, "))")
  }
}

impl ErlApply {
  /// Creates a new function call (application) AST node
  pub fn new(location: SourceLoc, expr: Arc<ErlAst>, args: Vec<Arc<ErlAst>>) -> Self {
    ErlApply { location, expr, args }
  }

  /// Check the apply target it must be a callable.
  /// Check the apply arguments, they must match the arguments of the callable, or at least arity.
  /// The return type of the callable will be the apply synthesis result.
  pub fn synthesize_application_type(&self, scope: &RwLock<Scope>) -> ErlResult<Arc<ErlType>> {
    // Synthesize target and check its a function type
    let target_ty = self.expr.synthesize(scope)?;
    if !target_ty.is_function() {
      let msg = format!("Attempt to call a value which is not a function: {}", self.expr);
      return Err(TypeError::NotAFunction { msg }.into());
    }

    // Build argument type list and check every argument vs the target (the callee)
    let arg_types_r: ErlResult<Vec<Arc<ErlType>>> = self.args.iter()
        .map(|arg| arg.synthesize(scope))
        .collect();
    let arg_types = arg_types_r?;

    match target_ty.deref() {
      ErlType::Atom => unimplemented!("Callable is a local module function"),
      ErlType::Tuple { .. } => unimplemented!("Callable is a tuple"),

      // AnyFn is always callable and always returns any, for we do not know better
      ErlType::AnyFn => Ok(ErlType::any()),

      ErlType::Fn(fn_type) => self.synthesize_call_to_fn(fn_type, &arg_types),
      ErlType::FnRef { .. } => unimplemented!("Callable is a fun reference"),
      ErlType::Lambda => unimplemented!("Callable is a lambda"),

      other => {
        let msg = format!("Attempt to call a non-function: {}", other);
        Err(TypeError::NotAFunction { msg }.into())
      }
    }
    // let clause_type = FnClauseType::new(arg_types?, ret_ty).into();
    // let synthesized_t = ErlType::new_fn_type(vec![clause_type]).into();
    // Ok(synthesized_t)
  }

  fn synthesize_call_to_fn(&self, fn_type: &FnType,
                           arg_types: &[Arc<ErlType>]) -> ErlResult<Arc<ErlType>> {
    if self.args.len() != fn_type.arity() {
      let msg = format!(
        "Attempt to call a function with wrong number of arguments: expected {}, got {}",
        fn_type.arity(), self.args.len());
      return Err(TypeError::BadArity { msg }.into());
    }
    let compatible_clauses = fn_type.get_compatible_clauses(arg_types);
    if compatible_clauses.is_empty() {
      let args_str = self.args.iter()
          .map(|arg| format!("{}", arg))
          .collect::<Vec<String>>()
          .join(", ");
      let msg = format!("No compatible function clauses while calling {} with args ({})",
                        self.expr, args_str);
      return Err(TypeError::BadArguments { msg }.into());
    }
    // Return type only from compatible clauses
    let ret_types: Vec<Arc<ErlType>> = compatible_clauses.iter()
        .map(|fc| fc.ret_ty())
        .cloned()
        .collect();
    Ok(ErlType::new_union(&ret_types))
  }
}