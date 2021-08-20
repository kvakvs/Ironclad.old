//! Application (a function call on an expression)

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::typing::typevar::TypeVar;
use std::fmt::Formatter;
use crate::display;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_clause_type::FnClauseType;
use crate::typing::fn_type::FunctionType;

/// Contains a function call
pub struct Apply {
  /// Must resolve to a callable
  pub target: Box<CoreAst>,
  /// Must match arity
  pub args: Vec<CoreAst>,
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
  pub fn get_function_type(&self) -> ErlType {
    let arg_types: Vec<ErlType> = self.args.iter()
        .map(|arg| arg.get_type())
        .collect();
    let clause_type = FnClauseType::new(arg_types, self.ret_ty.into());
    let f_type = FunctionType::new(None, vec![clause_type]);
    ErlType::Fn(f_type)
  }
}