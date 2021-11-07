//! Synthesize a type from AST node
use std::sync::{Arc, RwLock};
use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::erl_error::{ErlError, ErlResult};
use crate::typing::erl_type::ErlType;
use crate::typing::scope::Scope;

/// Hosts code to synthesize ErlTypes from AST
pub struct TypeBuilder {}

impl TypeBuilder {
  /// From core AST subtree, create a type which we believe it will have, narrowest possible.
  /// It will be further narrowed later, if we don't happen to know at this moment.
  pub fn synthesize(env: &Arc<RwLock<Scope>>, node: &CoreAst) -> ErlResult<Arc<ErlType>> {
    match node {
      CoreAst::Empty => unreachable!("Should not be synthesizing type from empty AST nodes"),
      CoreAst::Module { .. } => unreachable!("Should not be synthesizing type from module node"),
      CoreAst::Attributes(_) => unreachable!("Should not be synthesizing type from module attrs"),
      CoreAst::ModuleFuns(_) => unreachable!("Should not be synthesizing type from module functions section"),
      CoreAst::FnDef(fndef) => fndef.synthesize_return_type(env),
      // CoreAst::FnRef { .. } => {}
      // CoreAst::Case(_) => {}
      // CoreAst::Let(_) => {}
      // CoreAst::Apply(_) => {}
      // CoreAst::Call(_) => {}
      // CoreAst::PrimOp { .. } => {}
      CoreAst::Var(v) => {
        if let Ok(env_read) = env.read() {
          match env_read.variables.get(&v.name) {
            None => return ErlError::variable_not_found(&v.name),
            Some(val) => Ok(val.ty.clone()),
          }
        } else {
          panic!("Can't read from env");
        }
      }
      CoreAst::Lit { value, .. } => Ok(ErlType::new_singleton(value)),
      CoreAst::BinOp { op, .. } => op.synthesize_type(env),
      // CoreAst::UnOp { .. } => {}
      CoreAst::List { elements, tail, .. } => {
        Self::synthesize_list_type(env, elements, tail)
      }
      CoreAst::Tuple { elements, .. } => {
        Self::synthesize_tuple_type(env, elements)
      }
      other => unimplemented!("Don't know how to synthesize type from {}", other),
    }
  }

  /// Having a list `[...]` AST node, try synthesize its type as precise as possible
  fn synthesize_list_type(env: &Arc<RwLock<Scope>>,
                          elements: &Vec<Arc<CoreAst>>,
                          tail: &Option<Arc<CoreAst>>) -> ErlResult<Arc<ErlType>> {
    let elements: ErlResult<Vec<Arc<ErlType>>> = elements.iter()
        .map(|el| el.synthesize_type(env))
        .collect();

    let synthesized_t = ErlType::StronglyTypedList {
      elements: elements?,
      tail: match tail {
        None => None,
        Some(t) => Some(t.synthesize_type(env)?)
      },
    }.into();

    Ok(synthesized_t)
  }

  /// Having a tuple `{...}` AST node, try synthesize its type as precise as possible
  fn synthesize_tuple_type(env: &Arc<RwLock<Scope>>, elements: &Vec<Arc<CoreAst>>) -> ErlResult<Arc<ErlType>> {
    let elements: ErlResult<Vec<Arc<ErlType>>> = elements.iter()
        .map(|el| el.synthesize_type(env))
        .collect();
    Ok(ErlType::Tuple { elements: elements? }.into())
  }
}
