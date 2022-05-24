//! Synthesize a type from AST node

use crate::syntax_tree::erl_ast::ErlAst;
use crate::syntax_tree::erl_error::ErlError;
use crate::typing::erl_type::ErlType;
use crate::typing::scope::Scope;
use ::function_name::named;
use libironclad_error::ic_error::IcResult;
use std::sync::{Arc, RwLock};

impl ErlAst {
  /// From AST subtree, create a type which we believe it will have, narrowest possible.
  /// It will be further narrowed later, if we don't happen to know at this moment.
  #[named]
  pub fn synthesize(&self, scope: &RwLock<Scope>) -> IcResult<Arc<ErlType>> {
    match self {
      ErlAst::Empty => unreachable!("Should not be synthesizing type from empty AST nodes"),
      ErlAst::ModuleStartAttr { .. } => {
        unreachable!("Should not be synthesizing type from module node")
      }
      ErlAst::FnDef(fndef) => fndef.synthesize_function_type(scope),
      ErlAst::FnRef { mfa, .. } => match Scope::retrieve_fn_from(scope, mfa) {
        None => ErlError::local_function_not_found(
          self.location(),
          mfa.clone(),
          format!("Function reference points to a non-existent local function: {}", mfa),
        ),
        Some(fndef) => Ok(fndef.as_fn_def().synthesize_function_type(scope)?),
      },
      ErlAst::Apply(apply) => apply.synthesize_application_type(scope),
      ErlAst::Var(v) => {
        if let Ok(env_read) = scope.read() {
          match env_read.variables.get(&v.name) {
            None => {
              println!("Var not found; Scope={:?}", &env_read);
              ErlError::variable_not_found(self.location(), v.name.clone())
            }
            Some(val) => Ok(val.clone()),
          }
        } else {
          panic!("{}: Can't find variable in the env: {}", function_name!(), v.name);
        }
      }
      ErlAst::Lit { value, .. } => Ok(ErlType::new_singleton(value)),
      ErlAst::BinaryOp { expr, .. } => expr.synthesize_binop_type(scope),
      ErlAst::List { elements, tail, .. } => Self::synthesize_list_type(scope, elements, tail),
      ErlAst::Tuple { elements, .. } => Self::synthesize_tuple_type(scope, elements),
      other => unimplemented!("Don't know how to synthesize type from {} debug {:?}", other, other),
    }
  }

  /// Having a list `[...]` AST node, try synthesize its type as precise as possible
  fn synthesize_list_type(
    env: &RwLock<Scope>,
    elements: &[Arc<ErlAst>],
    tail: &Option<Arc<ErlAst>>,
  ) -> IcResult<Arc<ErlType>> {
    let elements: IcResult<Vec<Arc<ErlType>>> =
      elements.iter().map(|el| el.synthesize(env)).collect();

    let synthesized_t = ErlType::StronglyTypedList {
      elements: elements?,
      tail: match tail {
        None => None,
        Some(t) => Some(t.synthesize(env)?),
      },
    }
    .into();

    Ok(synthesized_t)
  }

  /// Having a tuple `{...}` AST node, try synthesize its type as precise as possible
  fn synthesize_tuple_type(
    env: &RwLock<Scope>,
    elements: &[Arc<ErlAst>],
  ) -> IcResult<Arc<ErlType>> {
    let elements: IcResult<Vec<Arc<ErlType>>> =
      elements.iter().map(|el| el.synthesize(env)).collect();
    Ok(ErlType::Tuple { elements: elements? }.into())
  }
}
