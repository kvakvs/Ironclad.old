extern crate compiler;
extern crate function_name;

mod test_util;

use ::function_name::named;
use compiler::erl_error::ErlResult;
use std::ops::Deref;
use compiler::typing::erl_type::ErlType;
use compiler::mfarity::MFArity;
use compiler::project::module::Module;
use compiler::core_erlang::syntax_tree::core_ast::CoreAst;

#[named]
#[test]
fn synth_list_append() -> ErlResult<()> {
  test_util::start(function_name!(), "synthesize type for strongly typed list ++ another such");

  let module = Module::from_expr_source("[atom1] ++ [atom2]")?;
  let expr_type = module.core_ast.synthesize_type(&module.scope)?;
  println!("{}: Inferred {} 🡆 {}", function_name!(), &module.core_ast, expr_type);

  Ok(())
}

#[named]
#[test]
fn synth_simplefun_division() -> ErlResult<()> {
  test_util::start(function_name!(), "synthesize type for simple expression");

  let module = Module::from_fun_source("myfun(A) -> (A + 1) / 2.")?;
  let ast = module.core_ast.clone();

  match ast.deref() {
    CoreAst::FnDef(fn_def) => {
      // assert_eq!(fn_def.clauses.len(), 1, "FunctionDef must have exact one clause");
      assert_eq!(fn_def.funarity.arity, 1, "FnDef must have arity 1");
      assert_eq!(fn_def.funarity.name, "myfun", "FnDef's name must be myfun");
      // assert!(matches!(&fn_def.args[0], CoreAst::Var{..}), "FnDef's 1st arg must be a Var node");
    }
    other1 => test_util::fail_unexpected(other1),
  }
  println!("Parsed: {}", module.ast);

  // let f_t = ErlType::final_type(module.unifier.infer_ast(ast.deref()));
  let f_t = ast.synthesize_type(&module.scope)?;
  println!("{}: Inferred {} 🡆 {}", function_name!(), &ast, f_t);

  Ok(())
}

#[named]
#[test]
fn synth_simplefun_addition() -> ErlResult<()> {
  test_util::start(function_name!(), "synthesize type for a function(A) doing A+1");

  let module = Module::from_fun_source("myfun(A) -> A + 1.")?;
  let ast = module.core_ast.clone();
  let f_t = ast.synthesize_type(&module.scope)?;
  println!("{}: Inferred {} 🡆 {}", function_name!(), &ast, f_t);


  Ok(())
}

// #[named]
// #[test]
// /// Try synthesize type for a function which is a sum of two lists.
// /// Expected: Inferred type list(atom1|atom2)
// fn synth_atom_list_concatenation() -> ErlResult<()> {
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
// fn infer_fun_call_other_fun() -> ErlResult<()> {
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
fn synth_fun_call() -> ErlResult<()> {
  test_util::start(function_name!(), "synthesize type for a fun which calls another fun with a sum");
  let code = format!(
    "-module({}).\n\
    add(A, B) -> A + B.\n\
    main(A) -> add(A, 4).\n", function_name!());
  let module = Module::from_module_source(&code)?;
  println!("AST: {}", &module.core_ast);

  {
    let add_fn_ast = CoreAst::find_function_def(
      &module.core_ast, &MFArity::new_local("add", 2),
    ).unwrap();
    let add_fn_type = add_fn_ast.synthesize_type(&module.scope)?;
    println!("{}: Inferred for add fn {} 🡆 {}", function_name!(), add_fn_ast, add_fn_type);
  }

  {
    let main_fn_ast = CoreAst::find_function_def(
      &module.core_ast, &MFArity::new_local("main", 1),
    ).unwrap();
    let main_fn_type = main_fn_ast.synthesize_type(&module.scope)?;
    println!("{}: Inferred for main fn {} 🡆 {}", function_name!(), main_fn_ast, main_fn_type);
    // Expected: Main -> integer()
    assert!(ErlType::Number.is_subtype_of(&main_fn_type),
            "Function main/0 must have inferred type: number(); received {}", main_fn_type);
  }

  Ok(())
}

#[named]
#[test]
/// Synthesize type for a 2-clause function.
/// Expected: -spec main(one) -> 'atom1'; (two) -> 222.
fn synth_multiple_clause_test() -> ErlResult<()> {
  test_util::start(function_name!(), "synthesize type for a multi-clause function");
  let code = "-module(infer_multiple_clause).\n\
                   main(one) -> atom1;\n\
                   main(two) -> 222.\n";
  let module = Module::from_module_source(code)?;

  let main_fn_ast = CoreAst::find_function_def(
    &module.core_ast, &MFArity::new_local("main", 1),
  ).unwrap();

  // let main_ty = ErlType::final_type(module.unifier.infer_ast(find_result2.deref()));
  let main_ty = main_fn_ast.synthesize_type(&module.scope)?;
  println!("{}: Synthesized {} 🡆 {}", function_name!(), main_fn_ast, main_ty);

  // Assert that type is a two-clause function type
  if let ErlType::Fn(fntype) = main_ty.deref() {
    assert_eq!(fntype.clauses().len(), 2,
               "Synth type for main/1 must be a function type with two clauses, got {}", main_ty);
    let clause1 = fntype.clause(0);
    assert_eq!(clause1.arity(), 1);
    // assert!(clause1.args[0].eq(&ErlType::new_atom("one")),
    //         "Clause1 arg0 must be atom 'one', got {}", clause1.args[0]);
    assert!(clause1.ret_ty().eq(&ErlType::new_atom("atom1")),
            "Clause1 must return literal 'atom1', got {}", clause1.ret_ty());

    let clause2 = fntype.clause(1);
    assert_eq!(clause2.arity(), 1);
    // assert!(clause2.args[0].eq(&ErlType::new_atom("two")),
    //         "Clause2 arg0 must be atom 'two', got {}", clause2.args[0]);
    assert!(clause2.ret_ty().is_integer(),
            "Clause 2 must return '222' (or integer()), got {}", clause2.ret_ty());
  } else {
    panic!("Synth type for main/1 must be a function type with two clauses, got {}", main_ty);
  }

  Ok(())
}