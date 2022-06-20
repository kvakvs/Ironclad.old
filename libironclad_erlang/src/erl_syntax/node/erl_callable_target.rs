//! Defines a callable target, to use in function applications

use crate::erl_syntax::erl_ast::ast_iter::IterableAstNodeT;
use crate::erl_syntax::erl_ast::AstNode;
use crate::error::ic_error::IcResult;
use crate::project::module::mod_impl::ErlModule;
use crate::project::module::scope::scope_impl::Scope;
use crate::typing::erl_type::ErlType;
use libironclad_util::mfarity::MFArity;
use std::fmt::Formatter;
use std::sync::{Arc, RwLock};

/// A callable target (without application or args) to use in `ErlApply`.
#[derive(Debug, Clone)]
pub enum CallableTarget {
  /// An expression which is expected to resolve to a `mod:fun/arity`
  Expr(AstNode),
  /// A pointer to a function local or cross-module
  MFArity(MFArity),
  /// A `m:f/arity` or `f/arity` where module and function are not literal atoms, but some exprs
  MFAExpression {
    /// Possibly resolves to a module name
    module: Option<AstNode>,
    /// Resolves to a function name
    function: AstNode,
    /// Arity for `MFArity` creation
    arity: usize,
  },
  // TupleCall(),
}

impl std::fmt::Display for CallableTarget {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      CallableTarget::Expr(expr) => write!(f, "{}", expr),
      CallableTarget::MFArity(mfa) => match &mfa.module {
        Some(m) => write!(f, "{}:{}", m, mfa.name),
        None => write!(f, "{}", mfa.name),
      },
      CallableTarget::MFAExpression { module, function, .. } => match module {
        Some(m) => write!(f, "({}):({})", m, function),
        None => write!(f, "({})", function),
      },
    }
  }
}

impl CallableTarget {
  /// Creates a callable expression (without application or args) to use in `ErlApply`
  pub(crate) fn new_expr(expr: AstNode) -> Self {
    CallableTarget::Expr(expr)
  }

  /// Creates a callable mod:fun/arity (without application or args) to use in `ErlApply`
  pub(crate) fn new_mfa(mfa: MFArity) -> Self {
    CallableTarget::MFArity(mfa)
  }

  /// Creates a callable mod:fun/arity where mod and fun can be any expressions. Will try to check
  /// if mod and fun are literal atoms to simplify the callable target to `MFArity(mfa)` instead of
  /// a more complex `MFAExpression`.
  pub(crate) fn new_mfa_expr(m: Option<AstNode>, f: AstNode, a: usize) -> Self {
    if f.is_atom() {
      if m.is_none() {
        return Self::new_mfa(MFArity::new_local(f.as_atom(), a));
      }
      let module = m.unwrap();
      if module.is_atom() {
        return Self::new_mfa(MFArity::new(module.as_atom(), f.as_atom(), a));
      }
      // rewrap module
      return CallableTarget::MFAExpression { module: Some(module), function: f, arity: a };
    }
    CallableTarget::MFAExpression { module: m, function: f, arity: a }
  }

  /// Create a type for this callable target
  #[allow(dead_code)]
  pub(crate) fn synthesize(&self, module: &ErlModule, scope: &Scope) -> IcResult<Arc<ErlType>> {
    match self {
      CallableTarget::Expr(e) => e.synthesize(module, scope),
      CallableTarget::MFArity(_) => todo!("synthesize for mfarity callable target"),
      CallableTarget::MFAExpression { .. } => {
        todo!("synthesize for mfa-expression callable target")
      }
    }
  }
}

impl IterableAstNodeT for CallableTarget {
  fn children(&self) -> Option<Vec<AstNode>> {
    match self {
      CallableTarget::Expr(e) => e.children(),
      CallableTarget::MFArity(_) => None,
      CallableTarget::MFAExpression { module, function, .. } => {
        let mut result = function.children().unwrap_or_default();
        if let Some(m) = module {
          match m.children() {
            None => {}
            Some(m_children) => result.extend(m_children.iter().cloned()),
          }
        }
        Some(result)
      }
    }
  }
}
