mod test_util;

use ::function_name::named;
use libironclad_erlang::erl_syntax::parsers::token_stream::token_type::TokenType;
use libironclad_erlang::error::ic_error::IcResult;
use libironclad_erlang::typing::erl_integer::ErlInteger;
use libironclad_util::mfarity::MFArity;

// #[test]
// #[named]
// /// Try parse a simple pp directive -if(true).
// fn test_fragment_if_true() {
//   test_util::start(function_name!(), "Parse -if(true) directive");
//
//   let input = "-if(true).
// -endif.";
//   let _module = test_util::parse_module(function_name!(), input);
// }

#[test]
#[named]
#[should_panic]
/// Try parse a simple pp directive -if(3). and expect a panic
fn test_fragment_if_3() {
  test_util::start(function_name!(), "Parse -if(3) directive for a panic");
  let input = "-if(3).
-end.";
  let _module = test_util::parse_module(function_name!(), input);
  // assert!(result, "Parsing -if(3). must produce a panic");
}

#[test]
#[named]
/// Try how splitting module into directives and text works
fn parse_if_branches() {
  test_util::start(function_name!(), "Parse a module example with if(true)");
  let input = "-before_if.
-if(true).
-test_success.
-else.
-test_fail.
-endif.
-after_if().";
  let module = test_util::parse_module(function_name!(), input);
  let succ = module
    .root_scope
    .attributes
    .get(&"test_success".to_string())
    .unwrap();
  assert_eq!(succ.len(), 1);

  assert!(module
    .root_scope
    .attributes
    .contains(&"before_if".to_string()));
  assert!(module
    .root_scope
    .attributes
    .contains(&"test_success".to_string()));
  assert!(module
    .root_scope
    .attributes
    .contains(&"after_if".to_string()));
}

#[test]
#[named]
/// Try how splitting module into directives and text works; With comments
fn parse_if_block_with_comments() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a module example into fragments with comments");
  let input = "  -if(%true)
false).
-test_fail.
-else.
%%-endif.
-test_success().
-endif().";

  let module = test_util::parse_module(function_name!(), input);
  // assert_eq!(nodes.len(), 1, "Expect to only have 1 attribute: -test_success.");
  // assert!(nodes[0].is_generic_attr("test_success"));
  let root_scope = module.root_scope.clone();
  let success = root_scope
    .attributes
    .get(&"test_success".to_string())
    .unwrap_or_default();
  assert_eq!(success.len(), 1);
  Ok(())
}

#[test]
#[named]
fn parse_define_ident_only() {
  test_util::start(function_name!(), "Parse a basic -define macro with only ident");
  let input = "-define(AAA).
-ifdef(AAA).
-test_success.
-else.
-test_failure().
-endif.";
  let module = test_util::parse_module(function_name!(), input);
  let root_scope = module.root_scope.clone();
  let success = root_scope
    .attributes
    .get(&"test_success".to_string())
    .unwrap_or_default();
  let failure = root_scope
    .attributes
    .get(&"test_failure".to_string())
    .unwrap_or_default();
  assert_eq!(success.len(), 1, "-test_success attribute must be present");
  assert_eq!(failure.len(), 0, "-test_failure attribute must not be present");
  assert!(module.root_scope.is_defined("AAA"));
}

#[test]
#[named]
fn parse_define_undef() {
  test_util::start(function_name!(), "Parse a basic -define macro with -undef");
  let input = "-define(AAA(X), blep).
-undef(AAA).
-ifdef(AAA).
-test_failure().
-else.
-test_success.
-endif.";
  let module = test_util::parse_module(function_name!(), input);
  let root_scope = module.root_scope.clone();
  let success = root_scope
    .attributes
    .get(&"test_success".to_string())
    .unwrap_or_default();
  let failure = root_scope
    .attributes
    .get(&"test_failure".to_string())
    .unwrap_or_default();
  assert_eq!(success.len(), 1, "-test_success attribute must be present");
  assert_eq!(failure.len(), 0, "-test_failure attribute must not be present");
  assert!(!module.root_scope.is_defined("AAA"));
}

#[test]
#[named]
fn parse_if_elif() {
  test_util::start(function_name!(), "Parse a basic -define macro with -if/elif");
  let input = "-define(AAA).
-if(false).
-test_failure().
-elif(true).
-test_success.
-elif(false).
-test_failure().
-endif.";
  let module = test_util::parse_module(function_name!(), input);
  let root_scope = module.root_scope.clone();
  let success = root_scope
    .attributes
    .get(&"test_success".to_string())
    .unwrap_or_default();
  let failure = root_scope
    .attributes
    .get(&"test_failure".to_string())
    .unwrap_or_default();
  assert_eq!(success.len(), 1, "-test_success attribute must be present");
  assert_eq!(failure.len(), 0, "-test_failure attribute must not be present");
  assert!(module.root_scope.is_defined("AAA"));
}

#[test]
#[named]
fn parse_define_with_body_no_args() {
  test_util::start(function_name!(), "Parse a basic -define macro with body and no args");
  let input = "-define(BBB, [true)).";
  let module = test_util::parse_module(function_name!(), input);
  // let ast = module.ast.borrow().clone();
  // let nodes = ast.children().unwrap_or_default();
  let pdef = module
    .root_scope
    .defines
    .get(&MFArity::new_local("BBB", 0))
    .unwrap();
  assert_eq!(pdef.name, "BBB");
  assert!(pdef.tokens[0].is_tok(TokenType::SquareOpen));
  assert!(pdef.tokens[1].is_tok(TokenType::Atom("true".to_string())));
  assert!(pdef.tokens[2].is_tok(TokenType::ParClose));
}

#[test]
#[named]
fn parse_define_with_body_2_args() {
  test_util::start(function_name!(), "Parse a basic -define macro with body and 2 args");
  let input = "-define(CCC(X,y), 2args\nbody).
-ifdef(CCC).
-test_success.
-endif().";
  let module = test_util::parse_module(function_name!(), input);
  let root_scope = module.root_scope.clone();
  let success = root_scope
    .attributes
    .get(&"test_success".to_string())
    .unwrap_or_default();
  assert_eq!(success.len(), 1);
  assert!(module.root_scope.is_defined("CCC"));
}

// #[test]
// #[named]
// #[ignore = "Hard to test fake filename includes, rewrite to include real file on disk"]
// fn parse_include_varied_spacing_1() {
//   test_util::start(function_name!(), "Parse -include() with varied spaces and newlines");
//   let input = "-include (\n\"testinclude\").\n";
//   let nodes = test_util::parse_module_unwrap(function_name!(), input);
//   let file = nodes[0].as_preprocessor_include();
//   assert_eq!(file, "testinclude");
// }

// #[test]
// #[named]
// #[ignore = "Hard to test fake filename includes, rewrite to include real file on disk"]
// fn parse_include_varied_spacing_2() {
//   test_util::start(function_name!(), "Parse -include() with varied spaces and newlines");
//   let input = " - include(\"test\"\n).\n";
//   let nodes = test_util::parse_module_unwrap(function_name!(), input);
//   assert_eq!(nodes.len(), 1);
//   let path = nodes[0].as_preprocessor_include();
//   assert_eq!(path, "test");
// }

fn parse_define_varied_spacing_do(function_name: &str, input: &str) {
  let input2 = format!("{}\n-ifdef(TEST).\n-testsuccess.\n-endif.", input);
  let module = test_util::parse_module(function_name, &input2);

  assert!(
    module
      .root_scope
      .attributes
      .contains(&"testsuccess".to_string()),
    "Attributes in the module scope must contain -testsuccess"
  );
  assert_eq!(
    module.root_scope.attributes.len(),
    1,
    "Exactly one attribute must be defined in the module scope"
  );
}

#[test]
#[named]
fn parse_define_varied_spacing() {
  test_util::start(function_name!(), "Parse -define() directives with varied spacing");
  parse_define_varied_spacing_do(function_name!(), "- define(TEST, true).");
  parse_define_varied_spacing_do(function_name!(), "- define\n(TEST,\ntrue).");
  parse_define_varied_spacing_do(function_name!(), "   - define\n\n  (  TEST   ,  \ntrue).");
}

#[test]
#[named]
fn test_define_with_dquotes() {
  let input = "-define(AAA(X,Y), \"aaa\").\n";
  let module = test_util::parse_module(function_name!(), input);
  let pdef = module
    .root_scope
    .defines
    .get(&MFArity::new_local("AAA", 2))
    .unwrap();
  assert!(pdef.tokens[0].is_tok(TokenType::new_str("aaa")));
  assert_eq!(pdef.args, vec!["X", "Y"]);
}

// #[test]
// fn parse_if_test() {
//   let if0 = PreprocessState::from_source("-if(10>20).").unwrap();
//   if let PpAst::If(s) = if0.deref() {
//     assert_eq!(s, "10>20");
//   } else {
//     panic!("Parsing -if(10>20) failed, received {:?}", if0);
//   }
// }

#[test]
#[named]
/// Try parse a define macro where value contains another macro
fn test_macro_expansion_in_define() {
  test_util::start(function_name!(), "Parse a -define macro with another macro in value");
  let module =
    test_util::parse_module(function_name!(), "-define(AAA, test_success).\n-define(BBB, ?AAA).");
  let pdef = module
    .root_scope
    .defines
    .get(&MFArity::new_local("BBB", 0))
    .unwrap();
  assert_eq!(pdef.name, "BBB");
  if let TokenType::Atom(a) = &pdef.tokens[0].content {
    assert_eq!(
      a, "test_success",
      "Macro BBB must expand to 'test_success' and not macro invocation of ?AAA"
    );
  } else {
    panic!("pdef.tokens[0] is not an atom: {}", pdef.tokens[0]);
  }
}

#[test]
#[named]
/// Try parse an expression with a macro
fn test_macro_expansion_in_expr() {
  test_util::start(function_name!(), "Parse an expression with macro substitution");
  let module = test_util::parse_module(function_name!(), "-define(AAA, bbb).\nmyfun() -> ?AAA.");
  // let ast = module.ast.borrow().clone();
  // let nodes = ast.children().unwrap_or_default();
  let fndef = module
    .root_scope
    .fn_defs
    .get(&MFArity::new_local("myfun", 0))
    .unwrap();
  assert_eq!(fndef.as_fn_def().clauses.len(), 1);
  assert!(fndef.as_fn_def().clauses[0].body.is_atom_of("bbb"));
}

#[test]
#[named]
/// Try substitute a macro with more AST nodes in it
fn test_ast_macro0() {
  test_util::start(
    function_name!(),
    "(1) Substitute from macro completes the syntax and makes it parseable, using ?M no parens",
  );
  // TODO: This will not parse till the method of macro substitution is changed
  let input = "-define(M1, A:B:C ->).
myfunction1() ->
  try test
  catch ?M1 ok
  end.";
  let _nodes = test_util::parse_module_unwrap(function_name!(), input);
}

#[test]
#[named]
/// Try substitute a macro with more AST nodes in it
fn test_ast_macro1() {
  test_util::start(
    function_name!(),
    "(2) Substitute from macro completes the syntax and makes it parseable, using ?M() syntax",
  );
  // TODO: This will not parse till the method of macro substitution is changed
  let input = "-define(M2, A:B:C ->).
myfunction1() ->
  try test
  catch ?M2() ok
  end.";
  let _nodes = test_util::parse_module_unwrap(function_name!(), input);
}

#[test]
#[named]
/// Try substitute a macro with a keyword making the syntax parseable
fn test_ast_macro_with_keyword() {
  test_util::start(
    function_name!(),
    "(3) Substitute from macro completes the syntax and makes it parseable",
  );
  let input = "-define(M3, end).
myfunction2() ->
  begin ok ?M3.";
  let _nodes = test_util::parse_module_unwrap(function_name!(), input);
}

#[test]
#[named]
/// Try substitute a macro with args
fn test_ast_macro_args_substitution() {
  test_util::start(function_name!(), "Substitute a macro with arguments");
  let input = "-define(M(A,B), A + B).
-define(result, ?M(1,2)).";
  let module = test_util::parse_module(function_name!(), input);
  let pdef = module
    .root_scope
    .defines
    .get(&MFArity::new_local("result", 0))
    .unwrap();
  assert!(pdef.tokens[0].is_tok(TokenType::Integer(ErlInteger::Small(1))));
  assert!(pdef.tokens[1].is_tok(TokenType::Plus));
  assert!(pdef.tokens[2].is_tok(TokenType::Integer(ErlInteger::Small(2))));
  println!("{:?}", pdef);
}
