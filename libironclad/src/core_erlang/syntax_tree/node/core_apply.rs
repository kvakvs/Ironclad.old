//! Application (a function call on an expression)
#![cfg(coreast)]

use std::fmt::Formatter;
use std::ops::Deref;
use std::sync::{Arc, RwLock};

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::erl_error::{ErlError, ErlResult};
use libironclad_erlsyntax::typing::erl_type::ErlType;
use libironclad_erlsyntax::typing::fn_type::FnType;
use libironclad_erlsyntax::typing::scope::Scope;
use libironclad_erlsyntax::typing::type_error::TypeError;
use libironclad_util::pretty::Pretty;
use libironclad_util::source_loc::SourceLoc;

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
impl Apply {}
