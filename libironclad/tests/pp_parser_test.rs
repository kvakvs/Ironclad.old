extern crate libironclad_erlang;
extern crate libironclad_preprocessor;

mod test_util;

use ::function_name::named;
use libironclad::stage::preprocess::PreprocessState;
use libironclad_preprocessor::nom_parser::pp_parse_types::PreprocessorParser;
use nom::Finish;
use std::ops::Deref;

use libironclad_preprocessor::syntax_tree::pp_ast::PpAst;

#[test]
#[named]
/// Try parse a simple pp directive -if(Expr).
fn test_fragment_if() {
  test_util::start(function_name!(), "Parse -if() directive");
  let src = "-if(true).";

  let (tail, out) = PreprocessorParser::parse_fragments_collection(src)
    .finish()
    .unwrap();
  assert!(tail.is_empty(), "Not all input consumed: {}", tail);
  println!("Out={:?}", out);
}

#[test]
#[named]
/// Try how splitting module into directives and text works
fn parse_if_as_fragments() {
  test_util::start(function_name!(), "Parse a module example into fragments of text and pp");
  let src = "before_if\n-if(true).\non_true\n\n-else.\non_false\n-endif.\nafter_if";
  println!("Parsing «{}»", src);

  let (tail, out) = PreprocessorParser::parse_fragments_collection(src)
    .finish()
    .unwrap();
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
  // println!("Out={:?}", out);
}

#[test]
#[named]
/// Try how splitting module into directives and text works; With comments
fn test_fragments_with_comments() {
  test_util::start(function_name!(), "Parse a module example into fragments with comments");
  let src = "hello\n-if(%true)\nfalse).\ntest\n\n-else.\n%%-endif.";

  let (tail, out) = PreprocessorParser::parse_fragments_collection(src)
    .finish()
    .unwrap();
  println!("Out={:?}", out);
  assert!(tail.is_empty(), "Not all input consumed: {}", tail);
}

#[test]
#[named]
fn parse_basic_define() {
  test_util::start(function_name!(), "Parse a basic -define macro with 0 params");
  let src = "define(AAA, true)"; // leading - and trailing . are not parsed in the parse_define
  let ast = PreprocessState::parse_helper(src, PreprocessorParser::parse_define).unwrap();
  if let PpAst::Define { .. } = ast.deref() {
    // pass
  } else {
    panic!("Expected PpAst::Define, received {:?}", ast);
  }
}

#[test]
#[named]
/// Try parse a define macro where value contains another macro
fn test_macro_in_define() {
  test_util::start(function_name!(), "Parse a -define macro with another macro in value");
  let src = "-define(AAA, 1).\n-define(BBB, ?AAA).";

  let (tail, out) = PreprocessorParser::parse_fragments_collection(src)
    .finish()
    .unwrap();
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
