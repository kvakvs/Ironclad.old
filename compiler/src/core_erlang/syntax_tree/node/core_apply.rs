//! Application (a function call on an expression)
#![cfg(coreast)]

use std::fmt::Formatter;
use std::ops::Deref;
use std::sync::{Arc, RwLock};

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::display::Pretty;
use crate::erl_error::{ErlError, ErlResult};
use crate::source_loc::SourceLoc;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_type::FnType;
use crate::typing::scope::Scope;
use crate::typing::type_error::TypeError;

/// Contains a function call
#[cfg(coreast)]
#[derive(Debug)]
pub struct Apply {
  /// Source file pointer
  pub location: SourceLoc,
  /// Must resolve to a callable
  pub target: Arc<CoreAst>,
  /// Must match arity
  pub args: Vec<Arc<CoreAst>>,
}

#[cfg(coreast)]
impl std::fmt::Display for Apply {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "apply {} ", self.target)?;
    Pretty::display_paren_list(&self.args, f)
  }
}

#[cfg(coreast)]
impl Apply {
}
