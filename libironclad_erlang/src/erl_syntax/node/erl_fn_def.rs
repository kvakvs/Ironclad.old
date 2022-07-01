//! Define a ErlFnDef struct for a new function AST node
use crate::erl_syntax::erl_ast::ast_iter::IterableAstNodeT;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_fn_clause::ErlFnClause;
use crate::error::ic_error::IcResult;
use crate::project::module::module_impl::ErlModule;
use crate::project::module::scope::scope_impl::Scope;
use crate::source_loc::SourceLoc;
use crate::typing::erl_type::{ErlType, ErlTypeImpl};
use crate::typing::fn_clause_type::FnClauseType;
use crate::typing::fn_type::FnType;
use libironclad_util::mfarity::MFArity;

/// AST node which declares a new function. Contains function clauses. Names and arities on
/// all clauses must be equal and same as the function name.
#[derive(Debug)]
pub struct ErlFnDef {
  /// Source file pointer
  pub location: SourceLoc,
  /// Function name and arity, must be same for each clause (checked on clause insertion).
  /// Always local (`MFArity::module` is `None`)
  pub funarity: MFArity,
  /// Function clauses (non-empty vec)
  pub clauses: Vec<ErlFnClause>,
}

impl ErlFnDef {
  /// Create a new function definition AST node. Argument types vector is initialized with unions of
  /// all argument types.
  #[allow(dead_code)]
  pub(crate) fn new(location: SourceLoc, funarity: MFArity, clauses: Vec<ErlFnClause>) -> Self {
    assert!(!clauses.is_empty(), "Cannot construct a function definition without clauses");
    Self { location, funarity, clauses }
  }

  /// Produce `ErlType` for this function definition, with all clauses and their return types
  #[allow(dead_code)]
  pub(crate) fn synthesize_function_type(
    &self,
    module: &ErlModule,
    _scope: &Scope,
  ) -> IcResult<ErlType> {
    let clauses_r: IcResult<Vec<FnClauseType>> = self
      .clauses
      .iter()
      .map(|fnc| fnc.synthesize_clause_type(module, &fnc.scope))
      .collect();
    let clauses = clauses_r?;

    let fn_type = FnType::new(self.funarity.arity, &clauses);
    let synthesized_t = ErlTypeImpl::Fn(fn_type.into()).into();
    Ok(synthesized_t)
  }

  /// Produce a function return type, as union of all clauses returns
  #[allow(dead_code)]
  pub(crate) fn synthesize_return_type(
    &self,
    module: &ErlModule,
    _scope: &Scope,
  ) -> IcResult<ErlType> {
    // TODO: Filter out incompatible clauses
    let clauses_ret: IcResult<Vec<ErlType>> = self
      .clauses
      .iter()
      .map(|fnc| fnc.synthesize_clause_return_type(module, &fnc.scope))
      .collect();
    let synthesized_t = ErlTypeImpl::new_union(&clauses_ret?);
    Ok(synthesized_t)
  }
}

impl IterableAstNodeT for ErlFnDef {
  fn children(&self) -> Option<Vec<AstNode>> {
    let r: Vec<AstNode> = self
      .clauses
      .iter()
      .map(|fclause| fclause.body.clone())
      .collect();
    Some(r)
  }
}
