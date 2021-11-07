//! Application (a function call on an expression)
use std::fmt::Formatter;
use std::sync::{Arc, RwLock};

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::display::Pretty;
use crate::erl_error::{ErlError, ErlResult};
use crate::source_loc::SourceLoc;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_clause_type::FnClauseType;
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
  /// From argument types build a new ErlType::Function() with a single clause corresponding to
  /// that specific call `Apply` would be performing.
  /// TODO: This should synthesize return type not the expected fn type
  pub fn synthesize_type(&self, scope: &Arc<RwLock<Scope>>) -> ErlResult<Arc<ErlType>> {
    // Synthesize target and check its a function type
    let target_ty = self.target.synthesize_type(scope)?;
    if !target_ty.is_function() {
      let msg = format!("Attempt to call a value which is not a function: {}", self.target);
      return ErlError::type_error(TypeError::NotAFunction { msg });
    }

    let arg_types: ErlResult<Vec<Arc<ErlType>>> = self.args.iter()
        .map(|arg| TypeBuilder::synthesize(scope, arg))
        .collect();
    let ret_ty = ErlType::Any.into(); // TODO: Deduct here the expected types or something?

    let clause_type = FnClauseType::new(arg_types?, ret_ty).into();
    let synthesized_t = ErlType::new_fn_type(vec![clause_type]).into();
    Ok(synthesized_t)
  }
}