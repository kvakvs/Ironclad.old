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
  /// Checks whether synthesized type for expression `ast` contains ErlType `ty`.
  /// This is used to check (for example) whether an incoming value would be accepted by a function.
  /// This is essentially an Erlang "match" check.
  pub fn check(env: &Arc<RwLock<Scope>>, ast: &CoreAst, match_ty: &ErlType) -> ErlResult<bool> {
    let synth_type = ast.synthesize(env)?;

    println!("Checking AST type vs expected type\n\tAst: {}\n\tSynth type: {}\n\tExpected: {}",
             ast, synth_type, match_ty);

    if !match_ty.is_subtype_of(&synth_type) && !synth_type.is_subtype_of(match_ty) {
      ErlError::type_error(TypeError::ExpectedType {
        expected_type: format!("{}", match_ty),
        actual_type: format!("{}", synth_type),
      })
    } else {
      Ok(true)
    }
  }
}

// /// For function definition, check every clause if any return type is matching
// // TODO: narrowing, if we know incoming arg types, select only matching clauses
// fn check_fndef(fndef: &FnDef, match_ty: &ErlType) -> ErlResult<bool> {
//   let any_matches = fndef.clauses.iter()
//       .any(|clause| {
//         // rough unwrap, TODO: Maybe nicer error returning
//         Self::check(&clause.scope, &clause.body, match_ty).unwrap()
//       });
//   return Ok(any_matches);
// }
