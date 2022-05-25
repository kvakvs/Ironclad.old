extern crate libironclad_erlang;
extern crate libironclad_preprocessor;

mod test_util;

use ::function_name::named;
use libironclad::stage::preprocess::PreprocessState;
use libironclad_erlang::syntax_tree::nom_parse::misc::panicking_parser_error_reporter;
use libironclad_preprocessor::nom_parser::pp_parse_types::PreprocessorParser;
use nom::Finish;
use std::ops::Deref;

use libironclad_preprocessor::syntax_tree::pp_ast::PpAst;

#[test]
#[named]
/// Try parse a simple pp directive -if(Expr).
fn test_fragment_if() {
  test_util::start(function_name!(), "Parse -if() directive");
  let input = "-if(true)"; // no terminating .
  println!("In=«{}»", input);

  let (tail, result) =
    panicking_parser_error_reporter(input, PreprocessorParser::parse_if_temporary(input).finish());
  assert!(tail.is_empty(), "Not all input consumed: {}", tail);
  // assert_eq!(out.len(), 1, "Expecting exact one result");
  assert!(matches!(result.deref(), PpAst::_TemporaryIf(_ast)));
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

  let (tail, out) = panicking_parser_error_reporter(
    input,
    PreprocessorParser::parse_fragments_collection(input).finish(),
  );
  println!("Out={:?}", out);
  assert!(tail.is_empty(), "Not all input consumed: {}", tail);

  assert!(
    out[0].is_text_of("before_if"),
    "Expected text 'before_if', but got {:?}",
    out[0]
  );

  if let PpAst::IfBlock { cond_true, cond_false, .. } = out[1].deref() {
    let true_branch = cond_true
      .clone()
      .unwrap_or_else(|| panic!("must have true branch"));
    assert!(true_branch[0].is_text_of("on_true"));

    let false_branch = cond_false
      .clone()
      .unwrap_or_else(|| panic!("must have false branch"));
    assert!(false_branch[0].is_text_of("on_false"));
  } else {
    panic!("If block is expected, but got {:?}", out[1]);
  }

  assert!(out[2].is_text_of("after_if"), "Expected text 'after_if', but got {:?}", out[2]);
  todo!("Add an elseif test");
}

#[test]
#[named]
/// Try how splitting module into directives and text works; With comments
fn parse_if_block_with_comments() {
  test_util::start(function_name!(), "Parse a module example into fragments with comments");
  let input = "-if(%true)
false).
test\n
-else.
%%-endif.";
  println!("In=«{}»", input);

  let (tail, out) =
    panicking_parser_error_reporter(input, PreprocessorParser::parse_if_block(input).finish());
  println!("Out={:?}", out);
  assert!(tail.is_empty(), "Not all input consumed: {}", tail);
  todo!("Check that returned `if` contains 'false'");
}

#[test]
#[named]
fn parse_define_ident_only() {
  test_util::start(function_name!(), "Parse a basic -define macro with only ident");
  let input = "-define(AAA).";
  let ast = PreprocessState::parse_helper(input, PreprocessorParser::parse_define).unwrap();
  if let PpAst::Define { name, body, .. } = ast.deref() {
    assert_eq!(name, "AAA");
    assert!(body.is_none());
  } else {
    panic!("Expected Define, received {:?}", ast);
  }
}

#[test]
#[named]
fn parse_define_with_body_no_args() {
  test_util::start(function_name!(), "Parse a basic -define macro with body and no args");
  let input = "-define(BBB, [true)).";
  let ast = PreprocessState::parse_helper(input, PreprocessorParser::parse_define).unwrap();
  if let PpAst::Define { name, body, .. } = ast.deref() {
    assert_eq!(name, "BBB");
    assert!(body.is_some());
    assert_eq!(body.clone().unwrap(), "[true)");
  } else {
    panic!("Expected Define, received {:?}", ast);
  }
}

#[test]
#[named]
fn parse_define_with_body_2_args() {
  test_util::start(function_name!(), "Parse a basic -define macro with body and 2 args");
  let input = "-define(CCC(X,Y), 2args\nbody).";
  let ast = PreprocessState::parse_helper(input, PreprocessorParser::parse_define).unwrap();
  if let PpAst::Define { name, args, body, .. } = ast.deref() {
    assert_eq!(name, "CCC");
    assert!(args.is_some());
    assert_eq!(args.clone().unwrap().len(), 2);
    assert!(body.is_some());
    assert_eq!(body.clone().unwrap(), "2args\nbody");
  } else {
    panic!("Expected Define, received {:?}", ast);
  }
}

#[test]
#[named]
/// Try parse a define macro where value contains another macro
fn test_macro_in_define() {
  test_util::start(function_name!(), "Parse a -define macro with another macro in value");
  let input = "-define(AAA, 1).\n-define(BBB, ?AAA).";

  let (tail, out) = panicking_parser_error_reporter(
    input,
    PreprocessorParser::parse_fragments_collection(input).finish(),
  );
  assert!(tail.is_empty(), "Not all input consumed: {}", tail);

  println!("Out={:?}", out);
}

#[test]
#[named]
fn parse_include_varied_spacing() {
  test_util::start(function_name!(), "Parse -include() with varied spaces and newlines");
  let inc1 = PreprocessState::from_source("-include (\"test\").\n").unwrap();
  if let PpAst::File(ast) = inc1.deref() {
    assert_eq!(ast.len(), 1);
    if let PpAst::Include(t) = ast[0].deref() {
      assert_eq!(t, "test");
    } else {
      panic!("Expected File([Include]), received {:?}", ast);
    }
  }

  let inc2 = PreprocessState::from_source(" - include(\"test\"\n).\n").unwrap();
  if let PpAst::File(ast) = inc2.deref() {
    assert_eq!(ast.len(), 1);
    if let PpAst::Include(t) = ast[0].deref() {
      assert_eq!(t, "test");
    } else {
      panic!("Expected File([Include]), received {:?}", ast);
    }
  }

  let inc3 = PreprocessState::from_source("-include\n(\"test\"\n).\n").unwrap();
  if let PpAst::File(ast) = inc3.deref() {
    assert_eq!(ast.len(), 1);
    if let PpAst::Include(t) = ast[0].deref() {
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
  if let PpAst::File(ast) = d0.deref() {
    assert_eq!(ast.len(), 1);
    if let PpAst::Define { name, args, body } = ast[0].deref() {
      assert_eq!(name, "AAA");
      assert!(args.is_none());
      assert_eq!(body.clone().unwrap(), "\"aaa\"");
    } else {
      panic!(
        "Parsing define(AAA, \"aaa\"). failed, expected File([Define]), received {:?}",
        ast
      )
    }
  }

  let d1 = PreprocessState::from_source("-define(BBB, 666).").unwrap();
  if let PpAst::File(ast) = d1.deref() {
    assert_eq!(ast.len(), 1);
    if let PpAst::Define { name, args, body } = ast[0].deref() {
      assert_eq!(name, "BBB");
      assert!(args.is_none());
      assert_eq!(body.clone().unwrap(), "666");
    } else {
      panic!("Parsing define(BBB, 666). failed, expected File([Define]), received {:?}", ast)
    }
  }
}

#[test]
fn test_define_fun() {
  let d0 = PreprocessState::from_source("-define(AAA(X,Y), \"aaa\").\n").unwrap();
  if let PpAst::DefineFun { name, args, body } = d0.deref() {
    assert_eq!(name, "AAA");
    assert_eq!(*args, vec!["X", "Y"]);
    assert_eq!(body, "\"aaa\"");
  } else {
    panic!("Parsing -define() with args must return PpAst::DefineFun, received {:?}", d0)
  }
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
