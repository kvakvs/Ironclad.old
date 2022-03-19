extern crate compiler;

mod test_util;

use compiler::preprocessor::syntax_tree::pp_parser::{Rule};
use compiler::preprocessor::syntax_tree::pp_ast::{PpAst};
use crate::test_util::pp_parse;
use std::ops::Deref;

#[test]
/// Try parse string
fn parse_define0_test() {
  let define0 = pp_parse(Rule::pp_define, "-define(AAA, true).\n").unwrap();
  if let PpAst::Define(_name, _value) = define0.deref() {} else {
    panic!("Expected PpAst::Define, received {:?}", define0);
  }
}

#[test]
fn parse_include_test() {
  let inc1 = pp_parse(Rule::pp_include, "-include (\"test\").\n").unwrap();
  if let PpAst::Include(t) = inc1.deref() {
    assert_eq!(t, "test");
  } else {
    panic!("Expected PpAst::Include, received {:?}", inc1);
  }

  let inc2 = pp_parse(Rule::pp_include, "- include(\"test\"\n).\n").unwrap();
  if let PpAst::Include(t) = inc2.deref() {
    assert_eq!(t, "test");
  } else {
    panic!("Expected PpAst::Include, received {:?}", inc2);
  }

  let inc3 = pp_parse(Rule::pp_include, "-include\n(\"test\"\n).\n").unwrap();
  if let PpAst::Include(t) = inc3.deref() {
    assert_eq!(t, "test");
  } else {
    panic!("Expected PpAst::Include, received {:?}", inc3);
  }
}

#[test]
fn parse_define_test() {
  let d0 = pp_parse(Rule::pp_define, "- define(AAA, \"aaa\").").unwrap();
  if let PpAst::Define(name, val) = d0.deref() {
    assert_eq!(name, "AAA");
    assert_eq!(val, "\"aaa\"");
  } else {
    panic!("Parsing define(AAA, \"aaa\"). failed, received {:?}", d0)
  }

  let d1 = pp_parse(Rule::pp_define, "-define(BBB, 666).").unwrap();
  if let PpAst::Define(name, val) = d1.deref() {
    assert_eq!(name, "BBB");
    assert_eq!(val, "666");
  } else {
    panic!("Parsing define(BBB, 666). failed, received {:?}", d1)
  }
}

#[test]
fn parse_define_fun_test() {
  let d0 = pp_parse(Rule::pp_define_fun, "-define(AAA(X,Y), \"aaa\").\n").unwrap();
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
  let if0 = pp_parse(Rule::pp_if, "-if(10>20).").unwrap();
  if let PpAst::If(s) = if0.deref() {
    assert_eq!(s, "10>20");
  } else {
    panic!("Parsing -if(10>20) failed, received {:?}", if0);
  }
}
