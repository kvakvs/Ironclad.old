//! Application (a function call on an expression)
use std::fmt::Formatter;
use std::sync::Arc;

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::typing::typevar::TypeVar;
use crate::display;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_type::FunctionType;
use crate::source_loc::SourceLoc;
use crate::typing::fn_clause_type::FnClauseType;

/// Contains a function call
pub struct Apply {
  /// Source file pointer
  pub location: SourceLoc,
  /// Must resolve to a callable
  pub target: Arc<CoreAst>,
  /// Must match arity
  pub args: Vec<Arc<CoreAst>>,
  /// The unique typevar for return type
  pub ret_ty: TypeVar,
}

impl std::fmt::Display for Apply {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "apply {} ", self.target)?;
    display::display_paren_list(&self.args, f)
  }
}

impl Apply {
  /// From argument types build a new ErlType::Function() with a single clause corresponding to
  /// that specific call `Apply` would be performing
  pub fn get_function_type(&self) -> Arc<ErlType> {
    let arg_types: Vec<Arc<ErlType>> = self.args.iter()
        .map(|arg| arg.get_type())
        .collect();
    let clause_type = FnClauseType::new(arg_types, ErlType::TVar(self.ret_ty).into());
    let f_type = FunctionType::new(None, vec![clause_type]);
    ErlType::Fn(f_type).into()
  }
}