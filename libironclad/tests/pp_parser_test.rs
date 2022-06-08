mod test_util;

use ::function_name::named;
use libironclad::project::module::ErlModule;
use libironclad_erlang::erl_syntax::erl_ast::ast_iter::AstParentNodeT;
use libironclad_erlang::erl_syntax::parsers::defs::ParserInput;
use libironclad_erlang::erl_syntax::parsers::misc::panicking_parser_error_reporter;
use libironclad_erlang::erl_syntax::preprocessor::ast::PreprocessorNodeType;
use libironclad_erlang::erl_syntax::preprocessor::parsers::r#if::{
  parse_if_block, parse_if_directive,
};
use nom::Finish;
use std::path::PathBuf;

#[test]
#[named]
/// Try parse a simple pp directive -if(Expr).
fn test_fragment_if() {
  test_util::start(function_name!(), "Parse -if() directive");
  // let filename = PathBuf::from(function_name!());
  let input = "-if(true).";
  let parser_input = ParserInput::new_str(input);
  let (_tail, result) = panicking_parser_error_reporter(
    parser_input.clone(),
    parse_if_directive(parser_input).finish(),
  );
  let pp_node = result.as_preprocessor();
  assert!(matches!(pp_node, PreprocessorNodeType::_TemporaryIf(_ast)));
  println!("Out={:?}", result);
}

#[test]
#[named]
/// Try how splitting module into directives and text works
fn parse_if_as_fragments() {
  test_util::start(function_name!(), "Parse a module example into fragments of text and pp");
  let input = "-warning(\"before_if\").
-if(true).
-warning(\"on_true\").
-else.
-warning(\"on_false\").
-endif.
-warning(\"after_if\").";
  let nodes = test_util::parse_a_module(function_name!(), input);

  assert!(
    nodes[0].is_preprocessor_warning("before_if"),
    "Expected warning with a text 'before_if', but got {:?}",
    nodes[0]
  );

  if let PreprocessorNodeType::IfBlock { cond_true, cond_false, .. } = nodes[1].as_preprocessor() {
    assert_eq!(cond_true.len(), 1, "must have true branch");
    assert!(cond_true[0].is_preprocessor_warning("on_true"));

    assert_eq!(cond_false.len(), 1, "must have false branch");
    assert!(cond_false[0].is_preprocessor_warning("on_false"));
  } else {
    panic!("If block is expected, but got {:?}", nodes[1]);
  }

  assert!(
    nodes[2].is_preprocessor_warning("after_if"),
    "Expected text 'after_if', but got {:?}",
    nodes[2]
  );
  // TODO Add an elseif test
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
  let input = "-define(AAA).";
  let nodes = test_util::parse_a_module(function_name!(), input);
  let pp_node = nodes[0].as_preprocessor();
  if let PreprocessorNodeType::Define { name, body, .. } = pp_node {
    assert_eq!(name, "AAA");
    assert!(body.is_empty());
  } else {
    panic!("Expected Preprocessor::Define, received {:?}", pp_node);
  }
}

#[test]
#[named]
fn parse_define_with_body_no_args() {
  test_util::start(function_name!(), "Parse a basic -define macro with body and no args");
  let input = "-define(BBB, [true)).";
  let nodes = test_util::parse_a_module(function_name!(), input);
  assert_eq!(nodes.len(), 1);
  let pp_node = nodes[0].as_preprocessor();
  if let PreprocessorNodeType::Define { name, body, .. } = pp_node {
    assert_eq!(name, "BBB");
    assert_eq!(body, "[true)");
  } else {
    panic!("Expected Preprocessor::Define(BBB, [], '[true)'), received {:?}", pp_node);
  }
}

#[test]
#[named]
fn parse_define_with_body_2_args() {
  test_util::start(function_name!(), "Parse a basic -define macro with body and 2 args");
  let input = "-define(CCC(X,y), 2args\nbody).";
  let nodes = test_util::parse_a_module(function_name!(), input);
  assert_eq!(nodes.len(), 1);
  let pp_node = nodes[0].as_preprocessor();
  if let PreprocessorNodeType::Define { name, args, body, .. } = pp_node {
    assert_eq!(name, "CCC");
    assert!(!args.is_empty());

    assert_eq!(args.len(), 2);
    assert_eq!(args[0], "X");
    assert_eq!(args[1], "y");

    assert_eq!(body, "2args\nbody");
  } else {
    panic!(
      "Expected Preprocessor::Define(CCC, [X, Y], '2args\\nbody'), received {:?}",
      pp_node
    );
  }
}

#[test]
#[named]
/// Try parse a define macro where value contains another macro
fn test_macro_in_define() {
  test_util::start(function_name!(), "Parse a -define macro with another macro in value");
  let nodes =
    test_util::parse_a_module(function_name!(), "-define(AAA, bbb).\n-define(BBB, ?AAA).");
  assert_eq!(nodes.len(), 2);
  let (_, _, body) = nodes[1].as_preprocessor_define();
  assert_eq!(body, "bbb", "Macro ?AAA must expand to 'bbb'");
}

#[test]
#[named]
#[ignore]
fn parse_include_varied_spacing_1() {
  test_util::start(function_name!(), "Parse -include() with varied spaces and newlines");
  let input = "-include (\n\"testinclude\").\n";
  let nodes = test_util::parse_a_module(function_name!(), input);
  let file = nodes[0].as_preprocessor_include();
  assert_eq!(file, "testinclude");
}

#[test]
#[named]
fn parse_include_varied_spacing_2() {
  test_util::start(function_name!(), "Parse -include() with varied spaces and newlines");
  let input = " - include(\"test\"\n).\n";
  let nodes = test_util::parse_a_module(function_name!(), input);
  assert_eq!(nodes.len(), 1);
  let path = nodes[0].as_preprocessor_include();
  assert_eq!(path, "test");
}

fn parse_define_varied_spacing_do(
  function_name: &str,
  input: &str,
  match_macro: &str,
  match_text: &str,
) {
  let nodes = test_util::parse_a_module(function_name, input);
  assert_eq!(nodes.len(), 1);
  let (name, args, body) = nodes[0].as_preprocessor_define();
  assert_eq!(name, match_macro);
  assert!(args.is_empty());
  assert_eq!(body, match_text);
}

#[test]
#[named]
fn parse_define_varied_spacing() {
  test_util::start(function_name!(), "Parse -define() directives with varied spacing");
  parse_define_varied_spacing_do(function_name!(), "- define(AAA, \"aaa\").", "AAA", "\"aaa\"");
  parse_define_varied_spacing_do(function_name!(), "- define\n(BBB,\n666).", "BBB", "666");
  parse_define_varied_spacing_do(
    function_name!(),
    "   - define\n\n  (  CCC   ,  \nccc).",
    "CCC",
    "ccc",
  );
}

#[test]
#[named]
fn test_define_with_dquotes() {
  let input = "-define(AAA(X,Y), \"aaa\").\n";
  let nodes = test_util::parse_a_module(function_name!(), input);
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
