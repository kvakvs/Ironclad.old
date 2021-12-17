extern crate compiler;
extern crate function_name;

mod test_util;

use std::ops::Deref;
use ::function_name::named;
use compiler::core_erlang::syntax_tree::core_ast::CoreAst;
use compiler::erl_error::ErlResult;
use compiler::project::module::Module;
use compiler::typing::check::TypeCheck;
use compiler::typing::erl_type::ErlType;
use compiler::typing::scope::Scope;

#[named]
#[test]
fn typing_synth() -> ErlResult<()> {
  test_util::start(function_name!(), "Typing.Synth");

  {
    let env = Scope::new_root_scope(function_name!().to_string());
    let list1 = Module::from_expr_source("[3.14159265358979 , 2,atom]")?;
    let t = list1.core_ast.synthesize_type(&env)?;
    println!("Synth list1: {}", &t);
    if let ErlType::StronglyTypedList { elements, tail } = t.deref() {
      assert!(elements[0].is_float());
      assert!(elements[1].is_integer());
      assert!(elements[2].is_atom());

      let tail2 = tail.clone().unwrap_or_else(|| ErlType::nil());
      assert!(tail2.is_nil());
      assert!(tail2.is_list());
    } else {
      panic!("Expected: StronglyTypedList, got {}", t)
    }
  }

  {
    let env = Scope::new_root_scope(function_name!().to_string());
    let tup1 = Module::from_expr_source("{tuple, 1.2, 3, \"hello\"}")?;
    let t = tup1.core_ast.synthesize_type(&env)?;
    println!("Synth tup1: {}", &t);
    if let ErlType::Tuple { elements } = t.deref() {
      assert!(elements[0].is_lit_atom("tuple"), "t[0] - expected 'tuple', got {}", elements[0]);
      assert!(elements[1].is_float(), "t[1] - expected float, got {}", elements[1]);
      assert!(elements[2].is_integer(), "t[2] - expected integer, got {}", elements[2]);
      assert!(elements[3].is_list(), "t[3] - expected string, got {}", elements[3]);
    } else {
      panic!("Expected: Tuple, got {}", t)
    }
  }

  Ok(())
}

#[named]
#[test]
fn typing_expr_check_1() -> ErlResult<()> {
  test_util::start(function_name!(), "Typing.ExprCheck.Atom");

  let env = Scope::new_root_scope(function_name!().to_string());
  let expr1 = Module::from_expr_source("hello")?;
  assert!(TypeCheck::check(&ErlType::atom(), &env, &expr1.core_ast)?,
          "Parsed atom 'hello' must be subtype of atom()");
  Ok(())
}

#[named]
#[test]
fn typing_expr_check_2() -> ErlResult<()> {
  test_util::start(function_name!(), "Typing.ExprCheck.FnDef1");
  let env = Scope::new_root_scope(function_name!().to_string());
  let fn1 = Module::from_fun_source("myfun() -> 10 + 20.")?;
  assert!(matches!(fn1.core_ast.deref(), CoreAst::FnDef(_)), "Expected FnDef() received {:?}", fn1.core_ast);
  println!("Synth fn1: {}", fn1.core_ast.synthesize_type(&env)?);
  assert!(TypeCheck::check(&ErlType::integer(), &env, &fn1.core_ast)?,
          "Parsed fun() must be subtype of integer()");
  Ok(())
}

#[named]
#[test]
fn typing_expr_check_3() -> ErlResult<()> {
  test_util::start(function_name!(), "Typing.ExprCheck.FnDef2");
  let env = Scope::new_root_scope(function_name!().to_string());
  let fn2 = Module::from_fun_source("myfun2(A) -> 10.0 + A.")?;
  assert!(matches!(fn2.core_ast.deref(), CoreAst::FnDef(_)), "Expected FnDef() received {:?}", fn2.core_ast);
  println!("Synth fn2: {}", fn2.core_ast.synthesize_type(&env)?);
  Ok(())
}

#[named]
#[test]
fn typing_subtyping_bool() -> ErlResult<()> {
  test_util::start(function_name!(), "Typing.Subtyping.Bool");

  let test1_bool = ErlType::Boolean;
  let test1_atom = ErlType::Atom;
  let test1_true = ErlType::new_atom("true");

  assert!(test1_bool.is_subtype_of(&test1_atom));
  assert!(!test1_atom.is_subtype_of(&test1_bool));

  assert!(test1_true.is_subtype_of(&test1_atom));
  assert!(test1_true.is_subtype_of(&test1_bool));
  Ok(())
}

#[named]
#[test]
fn typing_subtyping_number() -> ErlResult<()> {
  test_util::start(function_name!(), "Typing.Subtyping.Number");
  let test2_int = ErlType::Integer;
  let test2_flt = ErlType::Float;
  let test2_num = ErlType::Number;

  assert!(test2_int.is_subtype_of(&test2_num)); // int() is subtype of number()
  assert!(!test2_num.is_subtype_of(&test2_int)); // number() is not subtype of int

  assert!(test2_flt.is_subtype_of(&test2_num)); // float() is subtype of number()
  Ok(())
}

#[named]
#[test]
fn typing_subtyping_list() -> ErlResult<()> {
  test_util::start(function_name!(), "Typing.Subtyping.List");

  let test3_any = ErlType::AnyList;
  let test3_l_num = ErlType::list_of(ErlType::number());
  let test3_l_flt = ErlType::list_of(ErlType::float());
  let test3_l_int = ErlType::list_of(ErlType::integer());

  assert!(test3_l_num.is_subtype_of(&test3_any)); // list(number()) is subtype of list()
  assert!(!test3_any.is_subtype_of(&test3_l_num)); // list() not subtype of list(number())

  assert!(test3_l_flt.is_subtype_of(&test3_any)); // list(float()) is subtype of list()
  assert!(!test3_any.is_subtype_of(&test3_l_flt)); // list() not subtype of list(float())

  assert!(test3_l_int.is_subtype_of(&test3_any)); // list(integer()) is subtype of list()
  assert!(!test3_any.is_subtype_of(&test3_l_int)); // list() not subtype of list(integer())

  Ok(())
}
