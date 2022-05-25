extern crate libironclad_erlang;
extern crate libironclad_preprocessor;

mod test_util;

use ::function_name::named;
use libironclad::stage::preprocess::PreprocessState;
use libironclad_erlang::syntax_tree::nom_parse::misc::panicking_parser_error_reporter;
use libironclad_error::ic_error::IcResult;
use libironclad_preprocessor::nom_parser::pp_parse_types::PreprocessorParser;
use nom::Finish;

use libironclad_preprocessor::syntax_tree::pp_ast::PpAstType;
use libironclad_preprocessor::syntax_tree::pp_ast::PpAstType::{Define, File, Include};

#[test]
#[named]
/// Try parse a simple pp directive -if(Expr).
fn test_fragment_if() {
  test_util::start(function_name!(), "Parse -if() directive");
  let input = "-if(true).";
  println!("In=«{}»", input);

  let (_tail, result) =
    panicking_parser_error_reporter(input, PreprocessorParser::if_directive(input).finish());
  // assert_eq!(out.len(), 1, "Expecting exact one result");
  assert!(matches!(&result.node_type, PpAstType::_TemporaryIf(_ast)));
  println!("Out={:?}", result);
}

#[test]
#[named]
/// Try how splitting module into directives and text works
fn parse_if_as_fragments() {
  test_util::start(function_name!(), "Parse a module example into fragments of text and pp");
  let input = "before_if
-if(true).
on_true
-else.
on_false
-endif.
after_if";
  println!("In=«{}»", input);

  let (_tail, out) = panicking_parser_error_reporter(
    input,
    PreprocessorParser::parse_fragments_collection(input).finish(),
  );
  println!("Out={:?}", out);

  assert!(
    out[0].is_text_of("before_if"),
    "Expected text 'before_if', but got {:?}",
    out[0]
  );

  if let PpAstType::IfBlock { cond_true, cond_false, .. } = &out[1].node_type {
    assert_eq!(cond_true.len(), 1, "must have true branch");
    assert!(cond_true[0].is_text_of("on_true"));

    assert_eq!(cond_false.len(), 1, "must have false branch");
    assert!(cond_false[0].is_text_of("on_false"));
  } else {
    panic!("If block is expected, but got {:?}", out[1]);
  }

  assert!(out[2].is_text_of("after_if"), "Expected text 'after_if', but got {:?}", out[2]);
  // TODO Add an elseif test
}

#[test]
#[named]
/// Try how splitting module into directives and text works; With comments
fn parse_if_block_with_comments() {
  test_util::start(function_name!(), "Parse a module example into fragments with comments");
  let input = "-if(%true)
false).
on_true\n
-else.
%%-endif.
on_false
-endif().";
  println!("In=«{}»", input);

  let (_tail, ast) =
    panicking_parser_error_reporter(input, PreprocessorParser::if_block(input).finish());
  println!("Out={:?}", ast);

  if let PpAstType::IfBlock { cond, cond_true, cond_false } = &ast.node_type {
    assert!(cond.is_atom());
    assert_eq!(cond.as_atom(), "false");

    assert_eq!(cond_true.len(), 1, "true branch must have exact one item");
    assert!(cond_true[0].is_text_of("on_true"));

    assert_eq!(cond_false.len(), 1, "false branch must have exact one item");
    assert!(cond_false[0].is_text_of("on_false"));
  } else {
    panic!("Expected PpAst::IfBlock, but received {:?}", ast);
  }
}

#[test]
#[named]
fn parse_define_ident_only() {
  test_util::start(function_name!(), "Parse a basic -define macro with only ident");
  let input = "-define(AAA).";
  let ast = PreprocessState::parse_helper(input, PreprocessorParser::define_directive).unwrap();
  if let Define { name, body, .. } = &ast.node_type {
    assert_eq!(name, "AAA");
    assert!(body.is_empty());
  } else {
    panic!("Expected Define, received {:?}", ast);
  }
}

#[test]
#[named]
fn parse_define_with_body_no_args() {
  test_util::start(function_name!(), "Parse a basic -define macro with body and no args");
  let input = "-define(BBB, [true)).";
  let ast = PreprocessState::parse_helper(input, PreprocessorParser::define_directive).unwrap();
  if let Define { name, body, .. } = &ast.node_type {
    assert_eq!(name, "BBB");
    assert_eq!(body, "[true)");
  } else {
    panic!("Expected Define(BBB, [], '[true)'), received {:?}", ast);
  }
}

#[test]
#[named]
fn parse_define_with_body_2_args() {
  test_util::start(function_name!(), "Parse a basic -define macro with body and 2 args");
  let input = "-define(CCC(X,y), 2args\nbody).";
  let ast = PreprocessState::parse_helper(input, PreprocessorParser::define_directive).unwrap();

  if let Define { name, args, body, .. } = &ast.node_type {
    assert_eq!(name, "CCC");
    assert!(!args.is_empty());

    assert_eq!(args.len(), 2);
    assert_eq!(args[0], "X");
    assert_eq!(args[1], "y");

    assert_eq!(body, "2args\nbody");
  } else {
    panic!("Expected Define(CCC, [X, Y], '2args\\nbody'), received {:?}", ast);
  }
}

#[test]
#[named]
/// Try parse a define macro where value contains another macro
fn test_macro_in_define() {
  test_util::start(function_name!(), "Parse a -define macro with another macro in value");
  let input = "-define(AAA, 1).\n-define(BBB, ?AAA).";

  let (_tail, out) = panicking_parser_error_reporter(
    input,
    PreprocessorParser::parse_fragments_collection(input).finish(),
  );

  println!("Out={:?}", out);
}

#[test]
#[named]
fn parse_include_varied_spacing_1() {
  test_util::start(function_name!(), "Parse -include() with varied spaces and newlines");
  let inc1 = PreprocessState::from_source("-include (\"test\").\n").unwrap();
  if let File(nodes) = &inc1.node_type {
    assert_eq!(nodes.len(), 2); // define and empty line
    if let Include(t) = &nodes[0].node_type {
      assert_eq!(t, "test");
    } else {
      panic!("Expected File([Include]), received {:?}", nodes);
    }
  }
}

#[test]
#[named]
fn parse_include_varied_spacing_2() {
  test_util::start(function_name!(), "Parse -include() with varied spaces and newlines");
  let inc2 = PreprocessState::from_source(" - include(\"test\"\n).\n").unwrap();
  if let File(ast) = &inc2.node_type {
    assert_eq!(ast.len(), 2); // define and empty line
    if let Include(t) = &ast[0].node_type {
      assert_eq!(t, "test");
    } else {
      panic!("Expected File([Include]), received {:?}", ast);
    }
  }
}

#[test]
#[named]
fn parse_include_varied_spacing_3() {
  test_util::start(function_name!(), "Parse -include() with varied spaces and newlines");
  let inc3 = PreprocessState::from_source("-include\n(\"test\"\n).\n").unwrap();
  if let File(ast) = &inc3.node_type {
    assert_eq!(ast.len(), 2); // define and empty line
    if let Include(t) = &ast[0].node_type {
      assert_eq!(t, "test");
    } else {
      panic!("Expected File([Include]), received {:?}", ast);
    }
  }
}

#[test]
#[named]
fn parse_define_varied_spacing() {
  test_util::start(function_name!(), "Parse -define() directives with varied spacing");

  let d0 = PreprocessState::from_source("- define(AAA, \"aaa\").").unwrap();
  if let File(ast) = &d0.node_type {
    assert_eq!(ast.len(), 1);
    if let Define { name, args, body } = &ast[0].node_type {
      assert_eq!(name, "AAA");
      assert!(args.is_empty());
      assert_eq!(body, "\"aaa\"");
    } else {
      panic!(
        "Parsing define(AAA, \"aaa\"). failed, expected File([Define]), received {:?}",
        ast
      )
    }
  }

  let d1 = PreprocessState::from_source("- define\n(BBB,\n666).").unwrap();
  if let File(ast) = &d1.node_type {
    assert_eq!(ast.len(), 1);
    if let Define { name, args, body } = &ast[0].node_type {
      assert_eq!(name, "BBB");
      assert!(args.is_empty());
      assert_eq!(body, "666");
    } else {
      panic!("Parsing define(BBB, 666). failed, expected File([Define]), received {:?}", ast)
    }
  }
}

#[test]
fn test_define_with_dquotes() -> IcResult<()> {
  let file_ast = PreprocessState::from_source("-define(AAA(X,Y), \"aaa\").\n").unwrap();
  if let File(nodes) = &file_ast.node_type {
    if let Define { name, args, body } = &nodes[0].node_type {
      assert_eq!(name, "AAA");
      assert_eq!(*args, vec!["X", "Y"]);
      assert_eq!(body, "\"aaa\"");
      return Ok(());
    }
  }

  panic!(
    "Parsing -define() with args expecting Define([AAA, [X, Y], '\"aaa\"'), received {:?}",
    file_ast
  )
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
