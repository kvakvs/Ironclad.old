mod test_util;

use ::function_name::named;
use libironclad_erlang::erl_syntax::erl_ast::ast_iter::IterableAstNodeT;
use libironclad_erlang::erl_syntax::erl_ast::node_impl::AstNodeType;
use libironclad_erlang::erl_syntax::parsers::defs::ParserInput;
use libironclad_erlang::erl_syntax::parsers::misc::panicking_parser_error_reporter;
use libironclad_erlang::erl_syntax::preprocessor::ast::PreprocessorNodeType;
use libironclad_erlang::erl_syntax::preprocessor::parsers::if_ifdef::{
  if_condition, parse_if_block,
};
use nom::Finish;

#[test]
#[named]
/// Try parse a simple pp directive -if(true).
fn test_fragment_if_true() {
  test_util::start(function_name!(), "Parse -if(true) directive");

  let input = "-if(true).";
  let parser_input = ParserInput::new_str(input);
  let (_tail, result) =
    panicking_parser_error_reporter(parser_input.clone(), if_condition(parser_input).finish());
  assert!(result, "Parsing -if(true). must produce a 'true'");
}

#[test]
#[named]
#[should_panic]
/// Try parse a simple pp directive -if(3). and expect a panic
fn test_fragment_if_3() {
  test_util::start(function_name!(), "Parse -if(3) directive for a panic");
  let input = "-if(3).";
  let parser_input = ParserInput::new_str(input);
  let (_tail, result) =
    panicking_parser_error_reporter(parser_input.clone(), if_condition(parser_input).finish());
  assert!(result, "Parsing -if(3). must produce a panic");
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
  let nodes = test_util::parse_module_unwrap(function_name!(), input);
  assert_eq!(
    nodes.len(),
    3,
    "Expected to have 3 nodes: -before_if, -test_success, and -after_if"
  );
  assert!(nodes[0].is_generic_attr("before_if"));
  assert!(nodes[1].is_generic_attr("test_success"));
  assert!(nodes[2].is_generic_attr("after_if"));
}

#[test]
#[named]
/// Try how splitting module into directives and text works; With comments
fn parse_if_block_with_comments() {
  test_util::start(function_name!(), "Parse a module example into fragments with comments");
  let input = "  -if(%true)
false).
-warning(\"on_true\"
).
-else.
%%-endif.
-warning(\"on_false\").
-endif().";
  println!("In=«{}»", input);

  let parser_input = ParserInput::new_str(input);
  let (_tail, ast) =
    panicking_parser_error_reporter(parser_input.clone(), parse_if_block(parser_input).finish());
  println!("Parsed={}", ast);

  let pp_node = ast.as_preprocessor();
  if let PreprocessorNodeType::IfBlock { cond, cond_true, cond_false } = pp_node {
    assert!(cond.is_atom());
    assert_eq!(cond.as_atom(), "false");

    assert_eq!(cond_true.len(), 1, "true branch must have exact one item");
    assert!(cond_true[0].is_preprocessor_warning("on_true"));

    assert_eq!(cond_false.len(), 1, "false branch must have exact one item");
    assert!(cond_false[0].is_preprocessor_warning("on_false"));
  } else {
    panic!("Expected PpAst::IfBlock, but received {:?}", ast);
  }
}

#[test]
#[named]
fn parse_define_ident_only() {
  test_util::start(function_name!(), "Parse a basic -define macro with only ident");
  let input = "-define(AAA).
-ifdef(AAA).
-testattr.
-endif.";
  let nodes = test_util::parse_module_unwrap(function_name!(), input);
  assert!(
    matches!(nodes[0].content, AstNodeType::Empty { .. }),
    "-define must expand into empty node"
  );
  let (attr_tag, attr_val) = nodes[1].as_generic_attr();
  assert_eq!(attr_tag, "testattr");
  assert!(attr_val.is_none());
}

#[test]
#[named]
fn parse_define_with_body_no_args() {
  test_util::start(function_name!(), "Parse a basic -define macro with body and no args");
  let input = "-define(BBB, [true)).";
  let module = test_util::parse_module0(function_name!(), input);
  let nodes = module.ast.children().unwrap_or_default();
  assert_eq!(nodes.len(), 1);
  assert!(nodes[0].is_empty_ast_node(), "expecting an empty node transformed from -define");

  let pdef = module.parser_scope.get_value("BBB", 0).unwrap();
  assert_eq!(pdef.name, "BBB");
  assert_eq!(pdef.text, "[true)");
}

#[test]
#[named]
fn parse_define_with_body_2_args() {
  test_util::start(function_name!(), "Parse a basic -define macro with body and 2 args");
  let input = "-define(CCC(X,y), 2args\nbody).
-ifdef(CCC).
-testsuccess.
-endif().";
  let module = test_util::parse_module0(function_name!(), input);
  let nodes = module.ast.children().unwrap_or_default();

  assert_eq!(nodes.len(), 2, "1 line for -define and 1 line for -testsuccess");
  assert!(nodes[0].is_empty_ast_node(), "define node must be transformed into Empty");

  let (tag, _) = nodes[1].as_generic_attr();
  assert_eq!(tag, "testsuccess", "Expected a -testsuccess attribute for the test to pass");

  assert!(module.parser_scope.is_defined("CCC"));

  // let pp_node = nodes[0].as_preprocessor();
  // if let PreprocessorNodeType::Define { name, args, body, .. } = pp_node {
  //   assert_eq!(name, "CCC");
  //   assert!(!args.is_empty());
  //
  //   assert_eq!(args.len(), 2);
  //   assert_eq!(args[0], "X");
  //   assert_eq!(args[1], "y");
  //
  //   assert_eq!(body, "2args\nbody");
  // } else {
  //   panic!(
  //     "Expected Preprocessor::Define(CCC, [X, Y], '2args\\nbody'), received {:?}",
  //     pp_node
  //   );
  // }
}

#[test]
#[named]
fn parse_include_varied_spacing_1() {
  test_util::start(function_name!(), "Parse -include() with varied spaces and newlines");
  let input = "-include (\n\"testinclude\").\n";
  let nodes = test_util::parse_module_unwrap(function_name!(), input);
  let file = nodes[0].as_preprocessor_include();
  assert_eq!(file, "testinclude");
}

#[test]
#[named]
fn parse_include_varied_spacing_2() {
  test_util::start(function_name!(), "Parse -include() with varied spaces and newlines");
  let input = " - include(\"test\"\n).\n";
  let nodes = test_util::parse_module_unwrap(function_name!(), input);
  assert_eq!(nodes.len(), 1);
  let path = nodes[0].as_preprocessor_include();
  assert_eq!(path, "test");
}

fn parse_define_varied_spacing_do(function_name: &str, input: &str) {
  let input2 = format!("{}\n-ifdef(TEST).\n-testsuccess.\n-endif.", input);
  let nodes = test_util::parse_module_unwrap(function_name, &input2);

  assert_eq!(nodes.len(), 2);
  assert!(nodes[0].is_empty_ast_node()); // define node transformed into empty

  let (g_tag, g_val) = nodes[1].as_generic_attr();
  assert_eq!(g_tag, "testsuccess");
  assert!(g_val.is_none());
  // let (name, args, body) = nodes[0].as_preprocessor_define();
  // assert_eq!(name, match_macro);
  // assert!(args.is_empty());
  // assert_eq!(body, match_text);
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
  let nodes = test_util::parse_module_unwrap(function_name!(), input);
  assert_eq!(nodes.len(), 1);
  let (name, args, body) = nodes[0].as_preprocessor_define();
  assert_eq!(name, "AAA");
  assert_eq!(*args, vec!["X", "Y"]);
  assert_eq!(body, "\"aaa\"");
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
    test_util::parse_module0(function_name!(), "-define(AAA, bbb).\n-define(BBB, ?AAA).");
  // module.interpret_preprocessor_nodes().unwrap();
  let nodes = module.ast.children().unwrap_or_default();
  assert_eq!(
    nodes.len(),
    2,
    "Must parse to 2 empty nodes one for -define(AAA), and one for -define(BBB)"
  );
  assert!(nodes[0].is_empty_ast_node());
  assert!(nodes[1].is_empty_ast_node());
  // let (_, _, body) = nodes[1].as_preprocessor_define();
  // assert_eq!(body, "bbb", "Macro ?AAA must expand to «bbb» but is now «{}»", body);

  let pdef = module.parser_scope.get_value("BBB", 0).unwrap();
  assert_eq!(pdef.name, "BBB");
  assert_eq!(pdef.text, "bbb");
}

#[test]
#[named]
/// Try parse an expression with a macro
fn test_macro_expansion_in_expr() {
  test_util::start(function_name!(), "Parse an expression with macro substitution");
  let module = test_util::parse_module0(function_name!(), "-define(AAA, bbb).\nmyfun() -> ?AAA.");
  // module.interpret_preprocessor_nodes().unwrap();
  let nodes = module.ast.children().unwrap_or_default();
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
  let input = "-define(M2, end.).
myfunction2() ->
  begin ok ?M2";
  let _nodes = test_util::parse_module_unwrap(function_name!(), input);
}
