extern crate compiler;
extern crate function_name;

mod test_util;

use ::function_name::named;
use compiler::erl_error::ErlResult;
use compiler::project::module::Module;
use compiler::core_erlang::syntax_tree::core_ast::CoreAst;
use compiler::erlang::syntax_tree::nom_parse::parse_attr::nom_parse_generic_attr;
use compiler::mfarity::MFArity;

#[named]
#[test]
fn fn_generic_attr_parse() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a generic attribute line");

  {
    let attr1_src = "-fgsfds ffsmmm(GGG :: integer()) -> bbb().\n";
    // let attr1_mod = Module::from_module_attr_source(&attr1_src)?;
    let (tail1, result1) = nom_parse_generic_attr(attr1_src)?;
    assert!(tail1.is_empty(), "Not all input consumed from attr1_src, tail: {}", tail1);
    println!("ErlAst for attr1: {}", result1);
  }

  {
    let attr2_src = " -bbbggg (ababagalamaga () [] {{}}!!! --- ).\n";
    // let attr2_mod = Module::from_module_attr_source(&attr2_src)?;
    let (tail2, result2) = nom_parse_generic_attr(attr2_src)?;
    assert!(tail2.is_empty(), "Not all input consumed from attr2_src, tail: {}", tail2);
    println!("ErlAst for attr2: {}", result2);
  }
  Ok(())
}


#[named]
#[test]
fn fn_typespec_parse() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse typespec syntax for 1 and 2 clause fns");

  let module_src = format!("-module({}).
                                    -spec myfun(A :: integer()) -> any().
                                    myfun(A) -> (A + 1) / 2.",
                           function_name!());
  let module = Module::from_module_source_nom(&module_src)?;
  if let Ok(r_scope) = module.scope.read() {
    println!("Scope: {}", r_scope);
  }

  let myfun_ast = CoreAst::find_function_def(
    &module.core_ast, &MFArity::new_local("myfun", 1)).unwrap();

  // match myfun_ast.deref() {
  //   CoreAst::FnDef(fn_def) => {
  //     // assert_eq!(fn_def.clauses.len(), 1, "FunctionDef must have exact one clause");
  //     assert_eq!(fn_def.funarity.arity, 1, "FnDef must have arity 1");
  //     assert_eq!(fn_def.funarity.name, "myfun", "FnDef's name must be myfun");
  //     // assert!(matches!(&fn_def.args[0], CoreAst::Var{..}), "FnDef's 1st arg must be a Var node");
  //   }
  //   other1 => test_util::fail_unexpected(other1, "Expected CoreAst::FnDef"),
  // }
  println!("Core: {}", &module.core_ast);

  // let f_t = ErlType::final_type(module.unifier.infer_ast(ast.deref()));
  let synth_fn_type = myfun_ast.synthesize(&module.scope)?;
  println!("{}: Synthesized {} 🡆 {}", function_name!(), &myfun_ast, synth_fn_type);

  Ok(())
}
