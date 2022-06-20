//! Synthesize a type from AST node

use crate::erl_syntax::erl_ast::node_impl::AstNodeType::{
  Apply, BinaryOp, FnDef, FnRef, List, Lit, Tuple, Var,
};
use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::erl_error::ErlError;
use crate::error::ic_error::IcResult;
use crate::project::module::erl_module_root_scope;
use crate::project::module::mod_impl::ErlModule;
use crate::project::module::scope::scope_impl::Scope;
use crate::typing::erl_type::ErlType;
use ::function_name::named;
use std::sync::{Arc, RwLock};

impl AstNodeImpl {
  /// From AST subtree, create a type which we believe it will have, narrowest possible.
  /// It will be further narrowed later, if we don't happen to know at this moment.
  #[named]
  pub fn synthesize(&self, module: &ErlModule, scope: &Scope) -> IcResult<Arc<ErlType>> {
    match &self.content {
      AstNodeType::Empty { comment } => {
        unreachable!("Should not be synthesizing type from AST node: Empty({})", comment)
      }
      FnDef(fndef) => fndef.synthesize_function_type(module, scope),
      FnRef { mfa, .. } => match erl_module_root_scope(module).get_fn(mfa) {
        None => ErlError::local_function_not_found(
          self.location.clone(),
          mfa.clone(),
          format!("Function reference points to a non-existent local function: {}", mfa),
        ),
        Some(fndef) => Ok(fndef.as_fn_def().synthesize_function_type(module, scope)?),
      },
      Apply(apply) => apply.synthesize_application_type(self.location.clone(), module, scope),
      Var(v) => {
        if let Ok(r_vars) = scope.variables.read() {
          match r_vars.get(&v.name) {
            None => {
              println!("Var not found; Scope={:?}", &scope);
              ErlError::variable_not_found(self.location.clone(), v.name.clone())
            }
            Some(val) => Ok(val.clone()),
          }
        } else {
          panic!("{}: Can't find variable in the env: {}", function_name!(), v.name);
        }
      }
      Lit { value, .. } => Ok(ErlType::new_singleton(value)),
      BinaryOp { expr, .. } => expr.synthesize_binop_type(self.location.clone(), module, scope),
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
  ) -> IcResult<Arc<ErlType>> {
    let elements: IcResult<Vec<Arc<ErlType>>> = elements
      .iter()
      .map(|el| el.synthesize(module, scope))
      .collect();

    let synthesized_t = ErlType::StronglyTypedList {
      elements: elements?,
      tail: match tail {
        None => None,
        Some(t) => Some(t.synthesize(module, scope)?),
      },
    }
    .into();

    Ok(synthesized_t)
  }

  /// Having a tuple `{...}` AST node, try synthesize its type as precise as possible
  #[allow(dead_code)]
  fn synthesize_tuple_type(
    module: &ErlModule,
    scope: &Scope,
    elements: &[AstNode],
  ) -> IcResult<Arc<ErlType>> {
    let elements: IcResult<Vec<Arc<ErlType>>> = elements
      .iter()
      .map(|el| el.synthesize(module, scope))
      .collect();
    Ok(ErlType::Tuple { elements: elements? }.into())
  }
}
