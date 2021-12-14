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
fn infer_list_append() -> ErlResult<()> {
  test_util::start(function_name!(), "infer type for list++list");
  {
    let module = Module::new_parse_expr("[atom1] ++ [atom2]")?;
    let expr_type = module.core_ast.synthesize_type(&module.scope)?;
    println!("{}: Inferred {} 🡆 {}", function_name!(), &module.core_ast, expr_type);
  }
  Ok(())
}

#[named]
#[test]
fn infer_simplefun_division() -> ErlResult<()> {
  test_util::start(function_name!(), "infer type for simple expression");
  {
    let module = Module::new_parse_fun("myfun(A) -> (A + 1) / 2.")?;
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
  }
  Ok(())
}

#[named]
#[test]
fn infer_simplefun_addition() -> ErlResult<()> {
  {
    let module = Module::new_parse_fun("myfun(A) -> A + 1.")?;
    let ast = module.core_ast.clone();
    let f_t = ast.synthesize_type(&module.scope)?;
    println!("{}: Inferred {} 🡆 {}", function_name!(), &ast, f_t);
  }

  Ok(())
}

#[named]
#[test]
/// Try infer type for a function which is a sum of two lists.
/// Expected: Inferred type list(atom1|atom2)
fn infer_atom_list_concatenation() -> ErlResult<()> {
  test_util::start(function_name!(), "infer type for a sum of two lists with atoms");
  let module = Module::new_parse_fun("atomtest(A) -> [atom1] ++ [atom2].")?;

  let synthesized_t = module.core_ast.synthesize_type(&module.scope)?;
  println!("{}: Inferred {} 🡆 {}", function_name!(), &module.core_ast, synthesized_t);

  if let ErlType::StronglyTypedList { elements: all_elements, .. } = synthesized_t.deref() {
    assert_eq!(all_elements.len(), 1, "Strongly typed list must have length of 1");
    let first_element = all_elements[0].clone();

    if let ErlType::Union(u) = first_element.as_ref() {
      assert!(u.contains(&ErlType::new_atom("atom1")));
      assert!(u.contains(&ErlType::new_atom("atom2")));
    } else {
      panic!("{}: Inferred type expected: strongly typed list of [atom1|atom2], got a list of {}",
             function_name!(), first_element)
    }
  } else {
    panic!("{}: Inferred type expected: strongly typed list of [atom1|atom2], got {:?}",
           function_name!(), synthesized_t)
  };

  Ok(())
}

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
fn infer_fun_call_other_fun() -> ErlResult<()> {
  test_util::start(function_name!(), "infer type for a fun which calls another fun with a sum");
  let code = format!(
    "-module({}).\n\
    add(A, B) -> A + B.\n\
    main(A) -> add(A, 4).\n", function_name!());
  let module = Module::new_erl_module(&code)?;
  let main_fn_ast = CoreAst::find_function_def(
    &module.core_ast, &MFArity::new_local_str("main", 1),
  ).unwrap();

  // let f_t2 = ErlType::final_type(module.unifier.infer_ast(find_result2.deref()));
  let main_fn_type = main_fn_ast.synthesize_type(&module.scope)?;
  println!("{}: Inferred {} 🡆 {}", function_name!(), main_fn_ast, main_fn_type);

  // Expected: Main -> integer()
  assert!(ErlType::Number.is_subtype_of(&main_fn_type),
          "Function main/0 must have inferred type: number(); received {}", main_fn_type);

  Ok(())
}

#[named]
#[test]
fn infer_multiple_clause_test() -> ErlResult<()> {
  test_util::start(function_name!(), "infer type for a multi-clause function");
  let code = "-module(infer_multiple_clause).\n\
                   main(one) -> [atom1] ++ [atom2];\n\
                   main(two) -> 2 + 3.\n";
  let module = Module::new_erl_module(code)?;

  let find_result2 = CoreAst::find_function_def(
    &module.core_ast, &MFArity::new_local_str("main", 1),
  ).unwrap();

  // let main_ty = ErlType::final_type(module.unifier.infer_ast(find_result2.deref()));
  let main_ty = find_result2.synthesize_type(&module.scope)?;
  println!("{}: Inferred {} 🡆 {}", function_name!(), find_result2, main_ty);

  // Expected: Main -> number()|[atom1|atom2]
  let list_of_atom1atom2 = ErlType::list_of(
    ErlType::new_union(
      vec![ErlType::new_atom("atom1").into(),
           ErlType::new_atom("atom2").into()]
    )
  );
  // Assert that type is: number() | [atom1|atom2]
  if let ErlType::Union(u) = main_ty.deref() {
    assert_eq!(u.types().len(), 2, "Inferred type for main/0 must be union of size 2");
    assert!(!u.contains(&ErlType::Any), "Inferred type for main/0 must not contain any()");
    assert!(u.contains(&ErlType::Integer), "Inferred type for main/0 must contain integer()");
    assert!(u.contains(&list_of_atom1atom2), "Inferred type for main/0 must contain [atom1|atom2]");
  } else {
    panic!("Inferred type for main/0 must be a type union");
  }

  Ok(())
}