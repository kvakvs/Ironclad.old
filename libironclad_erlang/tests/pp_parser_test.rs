extern crate libironclad;

mod test_util;

use ::function_name::named;

use compiler::preprocessor::syntax_tree::pp_ast::{PpAst};
use std::ops::Deref;
use nom::Finish;
use compiler::preprocessor::nom_parser::PreprocessorParser;
use compiler::stage::preprocess::ErlPreprocessStage;

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
fn test_fragments() {
  test_util::start(function_name!(), "Parse a module example into fragments of text and pp");
  let src = "hello\n-if(true).\ntest\n\n-else.\n-endif.\n-module(test).";

  let (tail, out) = PreprocessorParser::parse_fragments_collection(src)
      .finish()
      .unwrap();
  assert!(tail.is_empty(), "Not all input consumed: {}", tail);
  println!("Out={:?}", out);
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
/// Try parse string
fn test_define0() {
  test_util::start(function_name!(), "Parse a basic -define macro with 0 params");
  let src = "define(AAA, true)"; // leading - and trailing . are not parsed in the parse_define
  let ast = ErlPreprocessStage::parse_helper(src,
                                             PreprocessorParser::parse_define).unwrap();
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
fn test_include() {
  let inc1 = ErlPreprocessStage::from_source("-include (\"test\").\n").unwrap();
  if let PpAst::Include(t) = inc1.deref() {
    assert_eq!(t, "test");
  } else {
    panic!("Expected PpAst::Include, received {:?}", inc1);
  }

  let inc2 = ErlPreprocessStage::from_source(" - include(\"test\"\n).\n").unwrap();
  if let PpAst::Include(t) = inc2.deref() {
    assert_eq!(t, "test");
  } else {
    panic!("Expected PpAst::Include, received {:?}", inc2);
  }

  let inc3 = ErlPreprocessStage::from_source("-include\n(\"test\"\n).\n").unwrap();
  if let PpAst::Include(t) = inc3.deref() {
    assert_eq!(t, "test");
  } else {
    panic!("Expected PpAst::Include, received {:?}", inc3);
  }
}

#[test]
fn test_define() {
  let d0 = ErlPreprocessStage::from_source("- define(AAA, \"aaa\").").unwrap();
  if let PpAst::Define { name, args, body } = d0.deref() {
    assert_eq!(name, "AAA");
    assert!(args.is_none());
    assert_eq!(body.clone().unwrap(), "\"aaa\"");
  } else {
    panic!("Parsing define(AAA, \"aaa\"). failed, received {:?}", d0)
  }

  let d1 = ErlPreprocessStage::from_source("-define(BBB, 666).").unwrap();
  if let PpAst::Define { name, args, body } = d1.deref() {
    assert_eq!(name, "BBB");
    assert!(args.is_none());
    assert_eq!(body.clone().unwrap(), "666");
  } else {
    panic!("Parsing define(BBB, 666). failed, received {:?}", d1)
  }
}

#[test]
fn test_define_fun() {
  let d0 = ErlPreprocessStage::from_source("-define(AAA(X,Y), \"aaa\").\n").unwrap();
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
//   let if0 = ErlPreprocessStage::from_source("-if(10>20).").unwrap();
//   if let PpAst::If(s) = if0.deref() {
//     assert_eq!(s, "10>20");
//   } else {
//     panic!("Parsing -if(10>20) failed, received {:?}", if0);
//   }
// }
