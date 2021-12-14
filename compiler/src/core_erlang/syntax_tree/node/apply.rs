//! Application (a function call on an expression)
use std::fmt::Formatter;
use std::ops::Deref;
use std::sync::{Arc, RwLock};

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::display::Pretty;
use crate::erl_error::{ErlError, ErlResult};
use crate::source_loc::SourceLoc;
use crate::typing::erl_type::ErlType;
use crate::typing::scope::Scope;
use crate::typing::type_error::TypeError;
use crate::typing::synth::TypeBuilder;

/// Contains a function call
#[derive(Debug)]
pub struct Apply {
  /// Source file pointer
  pub location: SourceLoc,
  /// Must resolve to a callable
  pub target: Arc<CoreAst>,
  /// Must match arity
  pub args: Vec<Arc<CoreAst>>,
}

impl std::fmt::Display for Apply {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "apply {} ", self.target)?;
    Pretty::display_paren_list(&self.args, f)
  }
}

impl Apply {
  /// Check the apply target it must be a callable.
  /// Check the apply arguments, they must match the arguments of the callable, or at least arity.
  /// The return type of the callable will be the apply synthesis result.
  pub fn synthesize_type(&self, scope: &Arc<RwLock<Scope>>) -> ErlResult<Arc<ErlType>> {
    // Synthesize target and check its a function type
    let target_ty = self.target.synthesize_type(scope)?;
    if !target_ty.is_function() {
      let msg = format!("Attempt to call a value which is not a function: {}", self.target);
      return ErlError::type_error(TypeError::NotAFunction { msg });
    }

    // Build argument type list and check every argument vs the target (the callee)
    let arg_types_r: ErlResult<Vec<Arc<ErlType>>> = self.args.iter()
        .map(|arg| TypeBuilder::synthesize(scope, arg))
        .collect();
    let arg_types = arg_types_r?;

    match target_ty.deref() {
      ErlType::Atom => unimplemented!("Callable is a local module function"),
      ErlType::Tuple { .. } => unimplemented!("Callable is a tuple"),

      // AnyFn is always callable and always returns any, for we do not know better
      ErlType::AnyFn => Ok(ErlType::Any.into()),

      ErlType::Fn(fn_type) => {
        if self.args.len() != fn_type.arity() {
          let msg = format!(
            "Attempt to call a function with wrong number of arguments: expected {}, got {}",
            fn_type.arity(), self.args.len());
          return ErlError::type_error(TypeError::BadArity { msg });
        }
        let compatible_clauses = fn_type.get_compatible_clauses(&arg_types);
        if compatible_clauses.is_empty() {
          let msg = "Attempt to call a function with incompatible arguments".to_string();
          return ErlError::type_error(TypeError::BadArguments { msg });
        }
        // Return type only from compatible clauses
        let ret_types = compatible_clauses.iter()
            .map(|fc| fc.ret_ty())
            .cloned()
            .collect();
        Ok(ErlType::new_union(ret_types))
      }
      ErlType::FnRef { .. } => unimplemented!("Callable is a fun reference"),
      ErlType::Lambda => unimplemented!("Callable is a lambda"),

      other => {
        let msg = format!("Attempt to call a non-function: {}", other);
        ErlError::type_error(TypeError::NotAFunction { msg })
      }
    }
    // let clause_type = FnClauseType::new(arg_types?, ret_ty).into();
    // let synthesized_t = ErlType::new_fn_type(vec![clause_type]).into();
    // Ok(synthesized_t)
  }
}