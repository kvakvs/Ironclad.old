extern crate compiler;
extern crate function_name;

mod test_util;

use ::function_name::named;
use compiler::erl_error::ErlResult;
use std::ops::Deref;
use compiler::project::module::Module;
use compiler::typing::erl_type::ErlType;
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
  assert!(sub1.is_subtype_of(&sup1));
  assert!(!sup1.is_subtype_of(&sub1));

  let sub2_int = ErlType::Integer;
  let sub2_flt = ErlType::Float;
  let sup2 = ErlType::Number;
  assert!(sub2_int.is_subtype_of(&sup2)); // int subtype of number
  assert!(sub2_flt.is_subtype_of(&sup2)); // float subtype of number
  assert!(!sup2.is_subtype_of(&sub2_int)); // number not subtype of int

  Ok(())
}
