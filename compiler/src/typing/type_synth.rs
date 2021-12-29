//! Synthesize a type from AST node

use ::function_name::named;
use std::sync::{Arc, RwLock};
use crate::erl_error::{ErlError, ErlResult};
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::typing::erl_type::ErlType;
use crate::typing::scope::Scope;

impl ErlAst {
  /// From AST subtree, create a type which we believe it will have, narrowest possible.
  /// It will be further narrowed later, if we don't happen to know at this moment.
  #[named]
  pub fn synthesize(&self, scope: &RwLock<Scope>) -> ErlResult<Arc<ErlType>> {
    match self {
      ErlAst::Empty => unreachable!("Should not be synthesizing type from empty AST nodes"),
      ErlAst::ModuleStartAttr { .. } => unreachable!("Should not be synthesizing type from module node"),
      ErlAst::FnDef(fndef) => fndef.synthesize_function_type(scope),
      ErlAst::FnRef { mfa, .. } => {
        match Scope::retrieve_fn_from(scope, mfa) {
          None => ErlError::local_function_not_found(mfa),
          Some(fndef) => Ok(fndef.as_fn_def().synthesize_function_type(scope)?),
        }
      }
      ErlAst::Apply(apply) => apply.synthesize_type(scope),
      ErlAst::Var(v) => {
        if let Ok(env_read) = scope.read() {
          match env_read.variables.get(&v.name) {
            None => ErlError::variable_not_found(&v.name),
            Some(val) => Ok(val.clone()),
          }
        } else {
          panic!("{}: Can't find variable in the env: {}", function_name!(), v.name);
        }
      }
      ErlAst::Lit { value, .. } => Ok(ErlType::new_singleton(value)),
      ErlAst::BinaryOp { expr, .. } => expr.synthesize_type(scope),
      ErlAst::List { elements, tail, .. } => {
        Self::synthesize_list_type(scope, elements, tail)
      }
      ErlAst::Tuple { elements, .. } => {
        Self::synthesize_tuple_type(scope, elements)
      }
      other => unimplemented!("Don't know how to synthesize type from {} debug {:?}", other, other),
    }
  }

  /// Having a list `[...]` AST node, try synthesize its type as precise as possible
  fn synthesize_list_type(env: &RwLock<Scope>,
                          elements: &[Arc<ErlAst>],
                          tail: &Option<Arc<ErlAst>>) -> ErlResult<Arc<ErlType>> {
    let elements: ErlResult<Vec<Arc<ErlType>>> = elements.iter()
        .map(|el| el.synthesize(env))
        .collect();

    let synthesized_t = ErlType::StronglyTypedList {
      elements: elements?,
      tail: match tail {
        None => None,
        Some(t) => Some(t.synthesize(env)?)
      },
    }.into();

    Ok(synthesized_t)
  }

  /// Having a tuple `{...}` AST node, try synthesize its type as precise as possible
  fn synthesize_tuple_type(env: &RwLock<Scope>, elements: &[Arc<ErlAst>]) -> ErlResult<Arc<ErlType>> {
    let elements: ErlResult<Vec<Arc<ErlType>>> = elements.iter()
        .map(|el| el.synthesize(env))
        .collect();
    Ok(ErlType::Tuple { elements: elements? }.into())
  }
}
