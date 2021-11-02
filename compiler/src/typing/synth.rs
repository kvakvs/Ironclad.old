//! Synthesize a type from AST node
use std::sync::Arc;
use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::typing::erl_type::ErlType;
use crate::typing::scope::Scope;

/// Hosts code to synthesize ErlTypes from AST
pub struct TypeBuilder {}

impl TypeBuilder {
  /// From core AST subtree, create a type which we believe it will have, narrowest possible.
  /// It will be further narrowed later, if we don't happen to know at this moment.
  pub fn synthesize(env: &Scope, node: &CoreAst) -> Arc<ErlType> {
    match node {
      CoreAst::Empty => unreachable!("Should not be synthesizing type from empty AST nodes"),
      CoreAst::Module { .. } => unreachable!("Should not be synthesizing type from module node"),
      CoreAst::Attributes(_) => unreachable!("Should not be synthesizing type from module attrs"),
      CoreAst::ModuleFuns(_) => unreachable!("Should not be synthesizing type from module functions section"),
      CoreAst::FnDef(body) => body.synthesize_return_type(env),
      // CoreAst::FnRef { .. } => {}
      // CoreAst::Case(_) => {}
      // CoreAst::Let(_) => {}
      // CoreAst::Apply(_) => {}
      // CoreAst::Call(_) => {}
      // CoreAst::PrimOp { .. } => {}
      CoreAst::Var(v) => {
        match env.variables.get(&v.name) {
          None => ErlType::None.into(), // TODO: must be a variable not found error
          Some(t) => t.clone(),
        }
      }
      CoreAst::Lit { value, .. } => ErlType::new_singleton(value),
      CoreAst::BinOp { op, .. } => op.synthesize_type(env),
      // CoreAst::UnOp { .. } => {}
      CoreAst::List { elements, tail, .. } => {
        Self::synthesize_list_type(env, elements, tail)
      },
      CoreAst::Tuple { elements, .. } => {
        Self::synthesize_tuple_type(env, elements)
      },
      other => unimplemented!("Don't know how to synthesize type from {}", other),
    }
  }

  /// Having a list `[...]` AST node, try synthesize its type as precise as possible
  fn synthesize_list_type(env: &Scope,
                          elements: &Vec<Arc<CoreAst>>,
                          tail: &Option<Arc<CoreAst>>) -> Arc<ErlType> {
    let elements = elements.iter()
        .map(|el| el.synthesize_type(env))
        .collect();
    ErlType::StronglyTypedList {
      elements,
      tail: match tail {
        None => None,
        Some(t) => Some(t.synthesize_type(env))
      },
    }.into()
  }

  /// Having a tuple `{...}` AST node, try synthesize its type as precise as possible
  fn synthesize_tuple_type(env: &Scope, elements: &Vec<Arc<CoreAst>>) -> Arc<ErlType> {
    let elements = elements.iter()
        .map(|el| el.synthesize_type(env))
        .collect();
    ErlType::Tuple { elements }.into()
  }
}
