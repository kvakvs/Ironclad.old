extern crate function_name;
extern crate libironclad_erlang;

mod test_util;

use ::function_name::named;
use libironclad_erlang::error::ic_error::IcResult;
use libironclad_erlang::project::module::module_impl::ErlModuleImpl;
use libironclad_erlang::project::module::scope::scope_impl::ScopeImpl;
use libironclad_erlang::typing::check::TypeCheck;
use libironclad_erlang::typing::erl_type::TypeImpl;
use std::ops::Deref;

#[named]
#[test]
fn typing_synth1() -> IcResult<()> {
  test_util::start(function_name!(), "Typing.Synth1");

  let module = ErlModuleImpl::new_default();
  let scope1 = ScopeImpl::new_root_scope(function_name!().to_string());
  let expr1 = test_util::parse_expr(function_name!(), "[3.14159265358979 , 2,atom]");
  let synth_t1 = expr1.synthesize(&module, &scope1)?;
  println!("Synth list1: {}", &synth_t1);

  if let TypeImpl::StronglyTypedList { elements, tail } = synth_t1.deref() {
    assert!(elements[0].is_float());
    assert!(elements[1].is_integer());
    assert!(elements[2].is_atom());

    let tail2 = tail.clone().unwrap_or_else(|| TypeImpl::nil());
    assert!(tail2.is_nil());
    assert!(tail2.is_list());
  } else {
    panic!("Expected: StronglyTypedList, got {}", synth_t1)
  }
  Ok(())
}

#[named]
#[test]
fn typing_synth2() -> IcResult<()> {
  test_util::start(function_name!(), "Typing.Synth2");

  let module = ErlModuleImpl::new_default();
  let scope2 = ScopeImpl::new_root_scope(function_name!().to_string());
  let expr2 = test_util::parse_expr(function_name!(), "{tuple_tag, 1.2, 3, \"hello\"}");
  let synth_t2 = expr2.synthesize(&module, &scope2)?;
  println!("Synth tup1: {}", &synth_t2);

  if let TypeImpl::Tuple { elements } = synth_t2.deref() {
    assert!(
      elements[0].is_lit_atom("tuple_tag"),
      "t[0] - expected 'tuple_tag', got {}",
      elements[0]
    );
    assert!(elements[1].is_float(), "t[1] - expected float, got {}", elements[1]);
    assert!(elements[2].is_integer(), "t[2] - expected integer, got {}", elements[2]);
    assert!(elements[3].is_list(), "t[3] - expected string, got {}", elements[3]);
  } else {
    panic!("Expected: Tuple, got {}", synth_t2)
  }

  Ok(())
}

#[named]
#[test]
fn typing_expr_check_1() -> IcResult<()> {
  test_util::start(function_name!(), "Typing.ExprCheck.Atom");
  let module = ErlModuleImpl::new_default();
  let scope = ScopeImpl::new_root_scope(function_name!().to_string());
  let expr = test_util::parse_expr(function_name!(), "hello");
  assert!(
    TypeCheck::check(&module, &scope, &expr, &TypeImpl::Atom)?,
    "Parsed atom 'hello' must be subtype of atom()"
  );
  Ok(())
}

#[named]
#[test]
/// Create a fun with 0 args, which returns an integer(). See if its compatible with an integer().
fn typing_expr_check_noarg() -> IcResult<()> {
  test_util::start(function_name!(), "Typing.ExprCheck.IntegerFun");

  let module = test_util::parse_module(function_name!(), "my_int_fun1() -> 10 + 20.");
  // let root_scope = module.root_scope;
  let scope1 = ScopeImpl::new_root_scope(function_name!().to_string());
  let match_ty = &TypeImpl::new_fn_type_of_any_args(0, TypeImpl::integer());
  let ast = module.ast.borrow().clone();
  assert!(
    TypeCheck::check(&module, &scope1, &ast, match_ty)?,
    "my_int_fun1()'s return type must be compatible with integer()"
  );
  Ok(())
}

#[named]
#[test]
/// Create a fun with argument, which returns an integer(). See if its compatible with an integer().
fn typing_check_int_arg_fn() -> IcResult<()> {
  test_util::start(function_name!(), "Typing.ExprCheck.IntegerFunWithArg");
  let scope = ScopeImpl::new_root_scope(function_name!().to_string());
  let module = test_util::parse_module(function_name!(), "my_int_fun2(A) -> 10 + A.");
  // assert!(nodes[0].is_fn_def(), "Expected FnDef() received {:?}", nodes);
  // println!("Synth my_int_fun2: {}", int_fn2.core_ast.synthesize(&env)?);
  let match_ty = &TypeImpl::new_fn_type_of_any_args(1, TypeImpl::integer());
  let ast = module.ast.borrow().clone();
  assert!(
    TypeCheck::check(&module, &scope, &ast, match_ty)?,
    "my_int_fun2()'s result type must be compatible with integer()"
  );
  Ok(())
}

#[named]
#[test]
/// Create a fun which returns a tuple of `{any(), integer()}` and see if it checks against a tuple
fn typing_expr_check_tuple1() -> IcResult<()> {
  test_util::start(function_name!(), "Typing.ExprCheck.TupleFun");
  let scope = ScopeImpl::new_root_scope(function_name!().to_string());
  let module = test_util::parse_module(function_name!(), "mytuple_fun(A) -> {A, 123}.");
  // assert!(nodes[0].is_fn_def(), "Expected FnDef() received {:?}", nodes[0]);
  // println!("Synth mytuple_fun: {}", tuple_fn.core_ast.synthesize(&env)?);
  let expected_type = TypeImpl::new_tuple(&vec![TypeImpl::any(), TypeImpl::integer()]);
  let match_ty = &TypeImpl::new_fn_type_of_any_args(1, expected_type);
  let ast = module.ast.borrow().clone();
  assert!(
    TypeCheck::check(&module, &scope, &ast, match_ty)?,
    "Parsed mytuple_fun(A) result type must match {{any(), integer()}}"
  );
  Ok(())
}

#[named]
#[test]
fn typing_subtyping_bool() -> IcResult<()> {
  test_util::start(function_name!(), "Typing.Subtyping.Bool");

  let test1_bool = TypeImpl::Boolean;
  let test1_atom = TypeImpl::Atom;
  let test1_true = TypeImpl::new_atom("true");

  assert!(test1_bool.is_subtype_of(&test1_atom));
  assert!(!test1_atom.is_subtype_of(&test1_bool));

  assert!(test1_true.is_subtype_of(&test1_atom));
  assert!(test1_true.is_subtype_of(&test1_bool));
  Ok(())
}

#[named]
#[test]
fn typing_subtyping_number() -> IcResult<()> {
  test_util::start(function_name!(), "Typing.Subtyping.Number");
  let test2_int = TypeImpl::Integer;
  let test2_flt = TypeImpl::Float;
  let test2_num = TypeImpl::Number;

  assert!(test2_int.is_subtype_of(&test2_num)); // int() is subtype of number()
  assert!(!test2_num.is_subtype_of(&test2_int)); // number() is not subtype of int

  assert!(test2_flt.is_subtype_of(&test2_num)); // float() is subtype of number()
  Ok(())
}

#[named]
#[test]
fn typing_subtyping_list() -> IcResult<()> {
  test_util::start(function_name!(), "Typing.Subtyping.List");

  let test3_any = TypeImpl::AnyList;
  let test3_l_num = TypeImpl::list_of(TypeImpl::number(), false);
  let test3_l_flt = TypeImpl::list_of(TypeImpl::float(), false);
  let test3_l_int = TypeImpl::list_of(TypeImpl::integer(), false);

  assert!(test3_l_num.is_subtype_of(&test3_any)); // list(number()) is subtype of list()
  assert!(!test3_any.is_subtype_of(&test3_l_num)); // list() not subtype of list(number())

  assert!(test3_l_flt.is_subtype_of(&test3_any)); // list(float()) is subtype of list()
  assert!(!test3_any.is_subtype_of(&test3_l_flt)); // list() not subtype of list(float())

  assert!(test3_l_int.is_subtype_of(&test3_any)); // list(integer()) is subtype of list()
  assert!(!test3_any.is_subtype_of(&test3_l_int)); // list() not subtype of list(integer())

  Ok(())
}
