extern crate function_name;
extern crate libironclad_erlang;
extern crate libironclad_util;

mod test_util;

use ::function_name::named;
use libironclad_erlang::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use libironclad_erlang::erl_syntax::erl_ast::node_impl::AstNodeType::FnDef;
use libironclad_erlang::erl_syntax::erl_op::ErlBinaryOp;
use libironclad_erlang::error::ic_error::IcResult;
use libironclad_erlang::project::erl_module::ErlModule;
use libironclad_erlang::typing::erl_type::ErlType;
use libironclad_util::mfarity::MFArity;
use std::ops::Deref;
use std::path::PathBuf;

#[named]
#[test]
fn synth_list_append() -> IcResult<()> {
  test_util::start(function_name!(), "synthesize type for strongly typed list ++ another such");

  let filename = PathBuf::from(function_name!());
  let parsed = ErlModule::from_expr_source(&filename, "[atom1] ++ [atom2]")?;
  let expr_type = parsed.ast.synthesize(&parsed.scope)?;

  println!("{}: Inferred {} 🡆 {}", function_name!(), &parsed.ast, expr_type);

  assert!(parsed.ast.is_binop(ErlBinaryOp::ListAppend));

  // let op = parsed.ast.as_binop();
  assert!(
    matches!(expr_type.deref(),
      ErlType::StronglyTypedList { elements, tail } if tail.is_none() && elements.len() == 2),
    "Synth type must be a strongly typed list without a tail"
  );

  Ok(())
}

#[named]
#[test]
fn synth_simplefun_division() -> IcResult<()> {
  test_util::start(function_name!(), "synthesize type for simple expression");

  let filename = PathBuf::from(function_name!());
  let parsed = ErlModule::from_fun_source(&filename, "myfun(A) -> (A + 1) / 2.")?;

  match &parsed.ast.content {
    FnDef(fn_def) => {
      // assert_eq!(fn_def.clauses.len(), 1, "FunctionDef must have exact one clause");
      assert_eq!(fn_def.funarity.arity, 1, "FnDef must have arity 1");
      assert_eq!(fn_def.funarity.name, "myfun", "FnDef's name must be myfun");
      // assert!(matches!(&fn_def.args[0], CoreAst::Var{..}), "FnDef's 1st arg must be a Var node");
    }
    other1 => test_util::fail_unexpected(other1),
  }
  println!("Parsed: {}", &parsed.ast);

  // let f_t = ErlType::final_type(module.unifier.infer_ast(ast.deref()));
  let f_t = parsed.ast.synthesize(&parsed.scope)?;
  println!("{}: Inferred {} 🡆 {}", function_name!(), &parsed.ast, f_t);

  Ok(())
}

#[named]
#[test]
fn synth_simplefun_addition() -> IcResult<()> {
  test_util::start(function_name!(), "synthesize type for a function(A) doing A+1");

  let filename = PathBuf::from(function_name!());
  let module = ErlModule::from_fun_source(&filename, "myfun(A) -> A + 1.")?;
  let synth_type = module.ast.synthesize(&module.scope)?;
  println!("{}: Inferred {} 🡆 {}", function_name!(), &module.ast, synth_type);

  Ok(())
}

// #[named]
// #[test]
// /// Try synthesize type for a function which is a sum of two lists.
// /// Expected: Inferred type list(atom1|atom2)
// fn synth_atom_list_concatenation() -> IcResult<()> {
//   test_util::start(function_name!(), "synthesize type for a sum of two lists with atoms");
//   let module = Module::from_fun_source("atomtest(A) -> [atom1] ++ [atom2].")?;
//
//   let synthesized_t = module.core_ast.synthesize_type(&module.scope)?;
//   println!("{}: Inferred {} 🡆 {}", function_name!(), &module.core_ast, synthesized_t);
//
//   if let ErlType::StronglyTypedList { elements: all_elements, .. } = synthesized_t.deref() {
//     assert_eq!(all_elements.len(), 1, "Strongly typed list must have length of 1");
//     let first_element = all_elements[0].clone();
//
//     if let ErlType::Union(u) = first_element.as_ref() {
//       assert!(u.contains(&ErlType::new_atom("atom1")));
//       assert!(u.contains(&ErlType::new_atom("atom2")));
//     } else {
//       panic!("{}: Inferred type expected: strongly typed list of [atom1|atom2], got a list of {}",
//              function_name!(), first_element)
//     }
//   } else {
//     panic!("{}: Inferred type expected: strongly typed list of [atom1|atom2], got {:?}",
//            function_name!(), synthesized_t)
//   };
//
//   Ok(())
// }

// #[named]
// #[test]
// fn infer_fun_call_other_fun() -> IcResult<()> {
//   test_util::start(function_name!(), "infer type for a fun which calls another fun with a sum");
//   let code = format!(
//     "-module({}).\n\
//     add(A, B) -> A + B.\n\
//     main() -> add(A, 4).\n", function_name!());
//   let module = Module::new_erl_module(&code)?;
//   let fn_add2 = CoreAst::find_function_def(
//     &module.core_ast, &MFArity::new_local_str("add", 2),
//   ).unwrap();
//
//   let type_fn_add2 = fn_add2.synthesize_type(&module.scope)?;
//   println!("{}: Inferred {} 🡆 {}", function_name!(), fn_add2, type_fn_add2);
//
//   // Expected: in Add/2 -> number(), args A :: number(), B :: integer()
//   assert!(ErlType::Number.is_subtype_of(&type_fn_add2),
//           "Function add/2 must have inferred type: number(), received {}", type_fn_add2);
//   Ok(())
// }

#[named]
#[test]
fn synth_fun_call() -> IcResult<()> {
  test_util::start(
    function_name!(),
    "synthesize type for a fun which calls another fun with a sum",
  );
  let filename = PathBuf::from(function_name!());

  let code = format!(
    "-module({}).
    add(A, B) -> A + B.
    main(A) -> add(A, 4).\n",
    function_name!()
  );
  let module = ErlModule::from_module_source(&filename, &code, None)?;
  // println!("Parsing: «{}»\nAST: {}", code, &module.ast);

  {
    let add_fn_ast =
      AstNodeImpl::find_function_def(&module.ast, &MFArity::new_local("add", 2)).unwrap();
    let add_fn_type = add_fn_ast.synthesize(&module.scope)?;
    println!("{}: Synthesized for add/2 {} 🡆 {}", function_name!(), add_fn_ast, add_fn_type);
  }

  {
    let main_fn_ast =
      AstNodeImpl::find_function_def(&module.ast, &MFArity::new_local("main", 1)).unwrap();
    let main_fn_type = main_fn_ast.synthesize(&module.scope)?;
    println!(
      "{}: Synthesized for main/1 {} 🡆 {}",
      function_name!(),
      main_fn_ast,
      main_fn_type
    );
    // Expected: Main -> integer()
    assert!(
      ErlType::Number.is_subtype_of(&main_fn_type),
      "For main/0 we expect synthesized type: fun(number()) -> number(); actual: {}",
      main_fn_type
    );
  }

  Ok(())
}

#[named]
#[test]
/// Synthesize type for a 2-clause function.
/// Expected: -spec main(one) -> 'atom1'; (two) -> 222.
fn synth_multiple_clause_test() -> IcResult<()> {
  test_util::start(function_name!(), "synthesize type for a multi-clause function");
  let source = format!(
    "-module({}).
    main(one) -> atom1;
    main(two) -> 222.",
    function_name!()
  );
  let filename = PathBuf::from(function_name!());
  let parsed = ErlModule::from_module_source(&filename, &source, None)?;

  let main_fn_ast =
    AstNodeImpl::find_function_def(&parsed.ast, &MFArity::new_local("main", 1)).unwrap();

  // let main_ty = ErlType::final_type(module.unifier.infer_ast(find_result2.deref()));
  let main_ty = main_fn_ast.synthesize(&parsed.scope)?;
  println!("{}: Synthesized {} 🡆 {}", function_name!(), main_fn_ast, main_ty);

  // Assert that type is a two-clause function type
  if let ErlType::Fn(fntype) = main_ty.deref() {
    assert_eq!(
      fntype.clauses().len(),
      2,
      "Synth type for main/1 must be a function type with two clauses, got {}",
      main_ty
    );
    let clause1 = fntype.clause(0);
    assert_eq!(clause1.arity(), 1);
    assert!(
      clause1.args[0].ty.eq(&ErlType::new_atom("one")),
      "Clause1 arg0 must be atom 'one', got {}",
      clause1.args[0]
    );
    assert!(
      clause1.ret_ty().eq(&ErlType::new_atom("atom1")),
      "Clause1 must return literal 'atom1', got {}",
      clause1.ret_ty()
    );

    let clause2 = fntype.clause(1);
    assert_eq!(clause2.arity(), 1);
    assert!(
      clause2.args[0].ty.eq(&ErlType::new_atom("two")),
      "Clause2 arg0 must be atom 'two', got {}",
      clause2.args[0]
    );
    assert!(
      clause2.ret_ty().is_integer(),
      "Clause 2 must return '222' (or integer()), got {}",
      clause2.ret_ty()
    );
  } else {
    panic!(
      "Synth type for main/1 must be a function type with two clauses, got {}",
      main_ty
    );
  }

  Ok(())

  // TODO: narrowing test, calling 2 clause function with 'one' and with 'two'
}
