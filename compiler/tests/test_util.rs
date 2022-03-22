use compiler::preprocessor::syntax_tree::pp_parser;
use compiler::erl_error::{ErlResult};
use compiler::preprocessor::syntax_tree::pp_ast::{PpAst, PpAstTree};
use compiler::project::source_file::SourceFile;
use std::path::PathBuf;
use pest::Parser;

#[allow(dead_code)]
pub fn pp_parse(rule: pp_parser::Rule, input: &str) -> ErlResult<Arc<PpAst>> {
  let parse_output = pp_parser::PpParser::parse(rule, input)?.next().unwrap();

  let sf = SourceFile::new(&PathBuf::from("<test>"), String::from(""));
  let tree = PpAstTree::new(sf, Rc::new(PpAst::Empty));

  tree.pp_parse_tokens_to_ast(parse_output)
}

#[allow(dead_code)]
pub fn fail_unexpected<T>(val: &T) where T: std::fmt::Display {
  panic!("Unexpected value: {}", val)
}

#[allow(dead_code)]
pub fn start(n: &str, descr: &str) {
  println!("--- TEST {}: START ({}) ---", n, descr);
}