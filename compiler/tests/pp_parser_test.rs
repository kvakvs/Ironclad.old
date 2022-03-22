extern crate compiler;

mod test_util;
use ::function_name::named;

use compiler::preprocessor::syntax_tree::pp_ast::{PpAst};
use std::ops::Deref;
use compiler::stage::preprocess::ErlPreprocessStage;

#[test]
#[named]
/// Try parse string
fn parse_define0_test() {
  test_util::start(function_name!(), "Parse a basic -define macro");
  let define0 = ErlPreprocessStage::from_source("-define(AAA, true).\n").unwrap();
  if let PpAst::Define { .. } = define0.deref() {
    // pass
  } else {
    panic!("Expected PpAst::Define, received {:?}", define0);
  }
}

#[test]
fn parse_include_test() {
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
fn parse_define_test() {
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
fn parse_define_fun_test() {
  let d0 = ErlPreprocessStage::from_source("-define(AAA(X,Y), \"aaa\").\n").unwrap();
  if let PpAst::DefineFun { name, args, body } = d0.deref() {
    assert_eq!(name, "AAA");
    assert_eq!(*args, vec!["X", "Y"]);
    assert_eq!(body, "\"aaa\"");
  } else {
    panic!("Parsing -define() with args must return PpAst::DefineFun, received {:?}", d0)
  }
}

#[test]
fn parse_if_test() {
  let if0 = ErlPreprocessStage::from_source("-if(10>20).").unwrap();
  if let PpAst::If(s) = if0.deref() {
    assert_eq!(s, "10>20");
  } else {
    panic!("Parsing -if(10>20) failed, received {:?}", if0);
  }
}
