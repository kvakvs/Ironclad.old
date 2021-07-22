use compiler::syntaxtree::pp::pp_parser;
use compiler::syntaxtree::erl::erl_parser;
use compiler::erl_error::{ErlResult, ErlError};
use compiler::syntaxtree::pp::pp_ast::{PpAst, PpAstTree};
use compiler::project::source_file::SourceFile;
use compiler::syntaxtree::erl::erl_ast::{ErlAst, ErlAstTree};
use std::sync::Arc;
use std::path::PathBuf;
use pest::Parser;
use std::rc::Rc;
use std::fmt::Debug;

#[allow(dead_code)]
pub fn pp_parse(rule: pp_parser::Rule, input: &str) -> ErlResult<Rc<PpAst>> {
  let parse_output = pp_parser::PpParser::parse(rule, input)?.next().unwrap();

  let sf = SourceFile::new(&PathBuf::from("<test>"), String::from(""));
  let tree = PpAstTree::new(Arc::new(sf), Rc::new(PpAst::Empty));

  tree.pp_parse_tokens_to_ast(parse_output)
}

#[allow(dead_code)]
pub fn erl_parse(rule: erl_parser::Rule, input: &str) -> ErlResult<Rc<ErlAst>> {
  let parse_output = match erl_parser::ErlParser::parse(rule, input) {
    Ok(mut root) => root.next().unwrap(),
    Err(bad) => {
      println!("Parse failed: {}", bad);
      assert!(false, "Parse failed");
      return Err(ErlError::from(bad));
    }
  };

  let sf = SourceFile::new(&PathBuf::from("<test>"), String::from(""));
  let tree = ErlAstTree::new(Arc::new(sf), Rc::new(ErlAst::Empty));

  tree.to_ast_single_node(parse_output)
}

#[allow(dead_code)]
pub fn test_module(filename: &str, input: &str) -> ErlAstTree {
  let mod_text = String::from("-module(test).\n") + input;
  match ErlAstTree::from_str(filename, &mod_text) {
    Ok(ast) => ast,
    Err(e) => {
      println!("Parse failed: {}", e);
      assert!(false, "Parse failed");
      Default::default()
    }
 }
}

#[allow(dead_code)]
pub fn fail_unexpected<T>(val: &T) where T: Debug {
  println!("Unexpected value: {:?}", val);
  panic!()
}