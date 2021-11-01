//! Application (a function call on an expression)
use std::fmt::Formatter;
use std::sync::Arc;

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::display;
use crate::source_loc::SourceLoc;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_clause_type::FnClauseType;
use crate::typing::scope::Scope;
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
    display::display_paren_list(&self.args, f)
  }
}

impl Apply {
  /// From argument types build a new ErlType::Function() with a single clause corresponding to
  /// that specific call `Apply` would be performing.
  /// TODO: This should synthesize return type not the expected fn type
  pub fn synthesize_type(&self, env: &Scope) -> Arc<ErlType> {
    let arg_types: Vec<Arc<ErlType>> = self.args.iter()
        .map(|arg| TypeBuilder::synthesize_from_core(env, arg))
        .collect();
    let ret_ty = ErlType::Any.into(); // TODO: Deduct here the expected types or something?

    let clause_type = FnClauseType::new(arg_types, ret_ty).into();
    ErlType::new_fn_type(vec![clause_type]).into()
  }
}