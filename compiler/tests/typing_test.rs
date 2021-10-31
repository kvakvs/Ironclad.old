extern crate compiler;
extern crate function_name;

mod test_util;

use ::function_name::named;
use compiler::erl_error::ErlResult;
use std::ops::Deref;
use compiler::project::module::Module;
use compiler::typing::erl_type::ErlType;
use compiler::typing::subtyping::SubtypeChecker;
use compiler::typing::synth::TypeBuilder;

#[named]
#[test]
fn typing_simple() -> ErlResult<()> {
  test_util::start(function_name!(), "Typing.Simple");
  let code = "hello+2";
  let mut module = Module::default();

  module.parse_erl_expr(code)?;

  let _ast = module.core_ast.clone();

  println!("Parsed: {}", module.ast);

  println!("Synthesized type: {:?}", TypeBuilder::synthesize_from_core(&module.core_ast).deref());

  Ok(())
}

#[named]
#[test]
fn typing_subtyping() -> ErlResult<()> {
  test_util::start(function_name!(), "Typing.Subtyping");

  let sub1 = ErlType::Boolean;
  let sup1 = ErlType::Atom;
  assert!(SubtypeChecker::is_subtype(&sub1, &sup1));
  assert!(!SubtypeChecker::is_subtype(&sup1, &sub1));

  let sub2_int = ErlType::Integer;
  let sub2_flt = ErlType::Float;
  let sup2 = ErlType::Number;
  assert!(SubtypeChecker::is_subtype(&sub2_int, &sup2)); // int subtype of number
  assert!(SubtypeChecker::is_subtype(&sub2_flt, &sup2)); // float subtype of number
  assert!(!SubtypeChecker::is_subtype(&sup2, &sub2_int)); // number not subtype of int

  Ok(())
}
