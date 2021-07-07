#![allow(clippy::upper_case_acronyms)]

#[derive(Parser)]
#[grammar = "syntaxtree/pp/pp_grammar.pest"]
pub struct PpParser;

#[cfg(test)]
mod tests {
  use crate::syntaxtree::pp::pp_parser::{Rule};
  // use pest::Parser;
  use crate::syntaxtree::pp::pp_ast::{PpAst};
  use crate::syntaxtree::test_util::pp_parse;

  #[test]
  /// Try s2_parse string
  fn parse_define0_test() {
    let define0 = pp_parse(Rule::pp_define, "-define(AAA, true).\n").unwrap();
    assert!(matches!(define0, PpAst::Define(_name, _value)));
  }

  #[test]
  fn parse_include_test() {
    let inc1 = pp_parse(Rule::pp_include, "-include (\"test\").\n").unwrap();
    assert_eq!(inc1, PpAst::Include("test".to_string()));

    let inc2 = pp_parse(Rule::pp_include, "- include(\"test\"\n).\n").unwrap();
    assert_eq!(inc2, PpAst::Include("test".to_string()));

    let inc3 = pp_parse(Rule::pp_include, "-include\n(\"test\"\n).\n").unwrap();
    assert_eq!(inc3, PpAst::Include("test".to_string()));
  }

  #[test]
  fn parse_define_test() {
    let d0 = pp_parse(Rule::pp_define, "- define(AAA, \"aaa\").").unwrap();
    match d0 {
      PpAst::Define(name, val) => {
        assert_eq!(name, "AAA");
        assert_eq!(val, "\"aaa\"");
      }
      _ => assert!(false, "Parsing define(AAA, \"aaa\"). failed"),
    }

    let d1 = pp_parse(Rule::pp_define, "-define(BBB, 666).").unwrap();
    match d1 {
      PpAst::Define(name, val) => {
        assert_eq!(name, "BBB");
        assert_eq!(val, "666");
      }
      _ => assert!(false, "Parsing define(BBB, 666). failed"),
    }
  }

  #[test]
  fn parse_define_fun_test() {
    let d0 = pp_parse(Rule::pp_define_fun, "-define(AAA(X,Y), \"aaa\").\n").unwrap();
    match d0 {
      PpAst::DefineFun { name, args, body } => {
        assert_eq!(name, "AAA");
        assert_eq!(args, vec!["X", "Y"]);
        assert_eq!(body, "\"aaa\"");
      }
      _ => assert!(false, "Parsing -define() with args must return PpAst::DefineFun")
    }
  }

  #[test]
  fn parse_if_test() {
    let if0 = pp_parse(Rule::pp_if, "-if(10>20).").unwrap();
    match if0 {
      PpAst::If(s) => { assert_eq!(s, "10>20"); }
      _ => assert!(false, "Parsing -if(10>20) failed")
    }
  }
}