//! Checks whether a type matches synthesized type for AST

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_error::ErlError;
use crate::error::ic_error::IcResult;
use crate::typing::erl_type::ErlType;
use crate::typing::scope::Scope;
use crate::typing::type_error::TypeError;
use std::sync::{Arc, RwLock};

/// Contains type checking code
pub struct TypeCheck {}

impl TypeCheck {
  /// Checks whether synthesized type for expression `ast` contains ErlType `ty`.
  /// This is used to check (for example) whether an incoming value would be accepted by a function.
  /// This is essentially an Erlang "match" check.
  pub fn check(
    env: &Arc<RwLock<Scope>>,
    ast: &AstNodeImpl,
    expected_ty: &ErlType,
  ) -> IcResult<bool> {
    let synthesized_ty = ast.synthesize(env)?;

    println!(
      "Checking AST type vs expected type\n\tAst: {}\n\tSynth type: {}\n\tExpected: {}",
      ast, synthesized_ty, expected_ty
    );

    if !synthesized_ty.is_subtype_of(expected_ty) {
      let type_err = TypeError::ExpectedType {
        expected_type: format!("{}", expected_ty),
        actual_type: format!("{}", synthesized_ty),
      };
      ErlError::type_error(&ast.location, type_err)
    } else {
      Ok(true)
    }
  }
}

// /// For function definition, check every clause if any return type is matching
// // TODO: narrowing, if we know incoming arg types, select only matching clauses
// fn check_fndef(fndef: &FnDef, match_ty: &ErlType) -> IcResult<bool> {
//   let any_matches = fndef.clauses.iter()
//       .any(|clause| {
//         // rough unwrap, TODO: Maybe nicer error returning
//         Self::check(&clause.scope, &clause.body, match_ty).unwrap()
//       });
//   return Ok(any_matches);
// }
