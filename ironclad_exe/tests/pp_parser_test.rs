mod test_util;

use ::function_name::named;
use libironclad_erlang::erl_syntax::erl_ast::ast_iter::IterableAstNodeT;
use libironclad_erlang::erl_syntax::token_stream::token_type::TokenType;
use libironclad_erlang::error::ic_error::IcResult;
use libironclad_util::mfarity::MFArity;

#[test]
#[named]
/// Try stage_parse a simple pp directive -if(true).
fn test_fragment_if_true() {
  test_util::start(function_name!(), "Parse -if(true) directive");

  let input = "-if(true).
-end.";
  let _module = test_util::parse_module(function_name!(), input);
}

#[test]
#[named]
#[should_panic]
/// Try stage_parse a simple pp directive -if(3). and expect a panic
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

  // assert!(nodes[0].is_generic_attr("before_if"));
  // assert!(nodes[1].is_generic_attr("test_success"));
  // assert!(nodes[2].is_generic_attr("after_if"));
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
  let ast = module.ast.borrow().clone();
  let nodes = ast.children().unwrap_or_default();
  assert_eq!(nodes.len(), 1);
  assert!(nodes[0].is_empty_ast_node(), "expecting an empty node transformed from -define");

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

  let attrs = module
    .root_scope
    .attributes
    .get(&"testsuccess".to_string())
    .unwrap();
  assert_eq!(attrs.len(), 1);
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
/// Try stage_parse a define macro where value contains another macro
fn test_macro_expansion_in_define() {
  test_util::start(function_name!(), "Parse a -define macro with another macro in value");
  let module = test_util::parse_module(function_name!(), "-define(AAA, bbb).\n-define(BBB, ?AAA).");
  let ast = module.ast.borrow().clone();
  let nodes = ast.children().unwrap_or_default();
  assert_eq!(
    nodes.len(),
    2,
    "Must stage_parse to 2 empty nodes one for -define(AAA), and one for -define(BBB)"
  );
  assert!(nodes[0].is_empty_ast_node());
  assert!(nodes[1].is_empty_ast_node());
  // let (_, _, body) = nodes[1].as_preprocessor_define();
  // assert_eq!(body, "bbb", "Macro ?AAA must expand to «bbb» but is now «{}»", body);

  let pdef = module
    .root_scope
    .defines
    .get(&MFArity::new_local("BBB", 0))
    .unwrap();
  assert_eq!(pdef.name, "BBB");
  assert!(pdef.tokens[0].is_tok(TokenType::new_str("bbb")));
}

#[test]
#[named]
/// Try stage_parse an expression with a macro
fn test_macro_expansion_in_expr() {
  test_util::start(function_name!(), "Parse an expression with macro substitution");
  let module = test_util::parse_module(function_name!(), "-define(AAA, bbb).\nmyfun() -> ?AAA.");
  let ast = module.ast.borrow().clone();
  let nodes = ast.children().unwrap_or_default();
  assert_eq!(nodes.len(), 2);
  let fndef = nodes[1].as_fn_def();
  assert_eq!(fndef.clauses.len(), 1);
  assert!(fndef.clauses[0].body.is_atom_of("bbb"));
}

#[test]
#[named]
/// Try substitute a macro with more AST nodes in it
fn test_ast_macro() {
  test_util::start(
    function_name!(),
    "(1) Substitute from macro completes the syntax and makes it parseable",
  );
  // TODO: This will not stage_parse till the method of macro substitution is changed
  let input = "-define(M1, A:B:C ->).
myfunction1() ->
  try test
  catch ?M1 ok
  end.";
  let _nodes = test_util::parse_module_unwrap(function_name!(), input);
}

#[test]
#[named]
/// Try substitute a macro with a keyword making the syntax parseable
fn test_ast_macro_with_keyword() {
  test_util::start(
    function_name!(),
    "(2) Substitute from macro completes the syntax and makes it parseable",
  );
  // TODO: This will not stage_parse till the method of macro substitution is changed
  let input = "-define(M2, end.).
myfunction2() ->
  begin ok ?M2";
  let _nodes = test_util::parse_module_unwrap(function_name!(), input);
}
