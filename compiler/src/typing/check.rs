//! Checks whether a type matches synthesized type for AST

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::erl_error::{ErlError, ErlResult};
use crate::typing::erl_type::ErlType;
use crate::typing::type_error::TypeError;

impl ErlType {
  /// Checks whether expression's synthesized type is a subtype of `self`
  pub fn is_supertype_of_expr(&self, ast: &CoreAst) -> ErlResult<bool> {
    let synth_type = ast.synthesize_type();
    if !synth_type.is_subtype_of(self) {
      ErlError::type_error(TypeError::ExprNotASubtype {
        ty: format!("{}", self),
        expr_ty: format!("{}", synth_type),
      })
    } else {
      Ok(true)
    }
  }
}