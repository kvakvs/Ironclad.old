//! Synthesize a type from AST node

use crate::erl_syntax::erl_ast::node_impl::AstNodeType::{
  Apply, BinaryOp, FnDef, FnRef, List, Lit, Tuple, Var,
};
use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::erl_error::ErlError;
use crate::error::ic_error::IcResult;
use crate::project::module::module_impl::ErlModule;
use crate::project::module::scope::scope_impl::Scope;
use crate::typing::erl_type::typekind::TypeKind;
use crate::typing::erl_type::{ErlType, TypeImpl};

impl AstNodeImpl {
  /// From AST subtree, create a type which we believe it will have, narrowest possible.
  /// It will be further narrowed later, if we don't happen to know at this moment.
  pub fn synthesize(&self, module: &ErlModule, scope: &Scope) -> IcResult<ErlType> {
    match &self.content {
      AstNodeType::Empty { comment } => {
        unreachable!("Should not be synthesizing type from AST node: Empty({})", comment)
      }
      FnDef(fndef) => fndef.synthesize_function_type(module, scope),
      FnRef { mfa, .. } => match module.root_scope.fn_defs.get(mfa) {
        None => ErlError::local_function_not_found(
          self.location.clone(),
          mfa.clone(),
          format!("Function reference points to a non-existent local function: {}", mfa),
        ),
        Some(fndef) => Ok(fndef.as_fn_def().synthesize_function_type(module, scope)?),
      },
      Apply(apply) => apply.synthesize_application_type(self.location.clone(), module, scope),
      Var(v) => match scope.variables.get(&v.name) {
        None => {
          println!("Var not found; Scope={:?}", &scope);
          ErlError::variable_not_found(self.location.clone(), v.name.clone())
        }
        Some(val) => Ok(val),
      },
      Lit { value, .. } => Ok(TypeImpl::new_unnamed(TypeKind::new_singleton(value.clone()))),
      BinaryOp { binop_expr: expr, .. } => {
        expr.synthesize_binop_type(self.location.clone(), module, scope)
      }
      List { elements, tail, .. } => Self::synthesize_list_type(module, scope, elements, tail),
      Tuple { elements, .. } => Self::synthesize_tuple_type(module, scope, elements),
      other => unimplemented!("Don't know how to synthesize type from {:?}", other),
    }
  }

  /// Having a list `[...]` AST node, try synthesize its type as precise as possible
  #[allow(dead_code)]
  fn synthesize_list_type(
    module: &ErlModule,
    scope: &Scope,
    elements: &[AstNode],
    tail: &Option<AstNode>,
  ) -> IcResult<ErlType> {
    let elements: IcResult<Vec<ErlType>> = elements
      .iter()
      .map(|el| el.synthesize(module, scope))
      .collect();

    let synthesized_t = TypeImpl::new_unnamed(TypeKind::StronglyTypedList {
      elements: elements?,
      tail: match tail {
        None => None,
        Some(t) => Some(t.synthesize(module, scope)?),
      },
    });

    Ok(synthesized_t)
  }

  /// Having a tuple `{...}` AST node, try synthesize its type as precise as possible
  #[allow(dead_code)]
  fn synthesize_tuple_type(
    module: &ErlModule,
    scope: &Scope,
    elements: &[AstNode],
  ) -> IcResult<ErlType> {
    let elements: IcResult<Vec<ErlType>> = elements
      .iter()
      .map(|el| el.synthesize(module, scope))
      .collect();
    Ok(TypeImpl::new_unnamed(TypeKind::Tuple { elements: elements? }))
  }
}
