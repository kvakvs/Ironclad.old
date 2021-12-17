//! Checks whether a type matches synthesized type for AST

use std::sync::{Arc, RwLock};
use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::erl_error::{ErlError, ErlResult};
use crate::typing::erl_type::ErlType;
use crate::typing::scope::Scope;
use crate::typing::type_error::TypeError;

/// Contains type checking code
pub struct TypeCheck {}

impl TypeCheck {
  /// Checks whether ErlType `ty` is a subtype of synthesized type for expression `ast`.
  /// This is used to check (for example) whether an incoming value would be accepted by a function.
  pub fn check(ty: &ErlType, env: &Arc<RwLock<Scope>>, ast: &CoreAst) -> ErlResult<bool> {
    let synth_type = ast.synthesize(env)?;

    if !ty.is_subtype_of(&synth_type) {
      ErlError::type_error(TypeError::ExpectedType {
        ty: format!("{}", synth_type),
        expr_ty: format!("{}", ty),
      })
    } else {
      Ok(true)
    }
  }

  // /// Checks whether `self` *contains* the expression's synthesized type
  // pub fn contains_expr_synth_type(&self, env: &Arc<RwLock<Scope>>, ast: &CoreAst) -> ErlResult<bool> {
  //   let synth_type = ast.synthesize_type(env)?;
  //
  //   if !synth_type.is_subtype_of(self) {
  //     ErlError::type_error(TypeError::ExprNotASubtype {
  //       ty: format!("{}", self),
  //       expr_ty: format!("{}", synth_type),
  //     })
  //   } else {
  //     Ok(true)
  //   }
  // }
}