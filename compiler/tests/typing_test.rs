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

  {
    let test1_bool = ErlType::Boolean;
    let test1_atom = ErlType::Atom;
    let test1_true = ErlType::new_atom("true");

    assert!(test1_bool.is_subtype_of(&test1_atom));
    assert!(!test1_atom.is_subtype_of(&test1_bool));

    assert!(test1_true.is_subtype_of(&test1_atom));
    assert!(test1_true.is_subtype_of(&test1_bool));
  }

  {
    let test2_int = ErlType::Integer;
    let test2_flt = ErlType::Float;
    let test2_num = ErlType::Number;

    assert!(test2_int.is_subtype_of(&test2_num)); // int() is subtype of number()
    assert!(!test2_num.is_subtype_of(&test2_int)); // number() is not subtype of int

    assert!(test2_flt.is_subtype_of(&test2_num)); // float() is subtype of number()
  }

  {
    let test3_any = ErlType::AnyList;
    let test3_l_num = ErlType::list_of(ErlType::Number);
    let test3_l_flt = ErlType::list_of(ErlType::Float);
    let test3_l_int = ErlType::list_of(ErlType::Integer);

    assert!(test3_l_num.is_subtype_of(&test3_any)); // list(number()) is subtype of list()
    assert!(!test3_any.is_subtype_of(&test3_l_num)); // list() not subtype of list(number())

    assert!(test3_l_flt.is_subtype_of(&test3_any)); // list(float()) is subtype of list()
    assert!(!test3_any.is_subtype_of(&test3_l_flt)); // list() not subtype of list(float())

    assert!(test3_l_int.is_subtype_of(&test3_any)); // list(integer()) is subtype of list()
    assert!(!test3_any.is_subtype_of(&test3_l_int)); // list() not subtype of list(integer())
  }

  Ok(())
}
