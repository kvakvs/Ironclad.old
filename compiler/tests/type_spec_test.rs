extern crate compiler;
extern crate function_name;

mod test_util;

use std::ops::Deref;
use std::path::PathBuf;
use ::function_name::named;
use compiler::erl_error::ErlResult;
use compiler::erlang::syntax_tree::erl_ast::ErlAst;
use compiler::project::module::Module;
use compiler::erlang::syntax_tree::nom_parse::parse_attr;
use compiler::mfarity::MFArity;

#[named]
#[test]
fn fn_generic_attr_parse() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a generic attribute line, consuming all as string");

  {
    let attr1_src = "-fgsfds ffsmmm(GGG :: integer()) -> bbb().\n";
    // let attr1_mod = Module::from_module_attr_source(&attr1_src)?;
    let (tail1, result1) = parse_attr::parse_generic_attr(attr1_src)?;
    assert!(tail1.is_empty(), "Not all input consumed from attr1_src, tail: {}", tail1);
    println!("ErlAst for attr1: {}", result1);
  }

  {
    let attr2_src = " -bbbggg (ababagalamaga () [] {{}}!!! --- ).\n";
    // let attr2_mod = Module::from_module_attr_source(&attr2_src)?;
    let (tail2, result2) = parse_attr::parse_generic_attr(attr2_src)?;
    assert!(tail2.is_empty(), "Not all input consumed from attr2_src, tail: {}", tail2);
    println!("ErlAst for attr2: {}", result2);
  }
  Ok(())
}


#[named]
#[test]
fn fn_typespec_parse() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse typespec syntax for 1 and 2 clause fns");

  let filename = PathBuf::from(function_name!());

  { //----------------------------------
    let spec1_src = format!("-spec myfun(A :: integer()) -> any().");
    let spec1_m = Module::from_fun_spec_source(&filename, &spec1_src)?;
    if let ErlAst::FnSpec { funarity, spec, .. } = spec1_m.ast.deref() {
      assert_eq!(funarity, &MFArity::new_local("myfun", 1),
                 "Expected fnspec for 'myfun'/1, got spec for {}", funarity);
      let fntype = spec.as_fn_type();
      assert_eq!(fntype.clauses().len(), 1, "Expected 1 clause in typespec, got {}", spec1_m.ast);
    } else {
      panic!("Expected AST FnSpec node, but got {}", spec1_m.ast)
    }
  }

  { //----------------------------------
    let spec2_src = format!("-spec x(A :: integer()) -> any(); (B :: atom()) -> C when C :: tuple().");
    let spec2_m = Module::from_fun_spec_source(&filename, &spec2_src)?;
    if let ErlAst::FnSpec { funarity, spec, .. } = spec2_m.ast.deref() {
      assert_eq!(funarity, &MFArity::new_local("myfun", 1),
                 "Expected fnspec for 'myfun'/1, got spec for {}", funarity);
      let fntype = spec.as_fn_type();
      assert_eq!(fntype.clauses().len(), 1, "Expected 1 clause in typespec, got {}", spec2_m.ast);
    } else {
      panic!("Expected AST FnSpec node, but got {}", spec2_m.ast)
    }
  }
  Ok(())
}
