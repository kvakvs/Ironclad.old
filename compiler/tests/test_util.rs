use compiler::syntaxtree::pp::pp_parser;
use compiler::syntaxtree::erl::erl_parser;
use compiler::erl_error::ErlResult;
use compiler::syntaxtree::pp::pp_ast::{PpAst, PpAstTree};
use compiler::project::source_file::SourceFile;
use compiler::syntaxtree::erl::erl_ast::{ErlAst, ErlAstTree};
use std::sync::Arc;
use std::path::PathBuf;
use pest::Parser;
use std::rc::Rc;

#[allow(dead_code)]
pub fn pp_parse(rule: pp_parser::Rule, input: &str) -> ErlResult<Rc<PpAst>> {
  let parse_output = pp_parser::PpParser::parse(rule, input)?.next().unwrap();

  let sf = SourceFile::new(&PathBuf::from("<test>"), String::from(""));
  let tree = PpAstTree::new(Arc::new(sf), Rc::new(PpAst::Empty));

  tree.pp_parse_tokens_to_ast(parse_output)
}

#[allow(dead_code)]
pub fn erl_parse(rule: erl_parser::Rule, input: &str) -> ErlResult<Rc<ErlAst>> {
  let parse_output = erl_parser::ErlParser::parse(rule, input)?.next().unwrap();

  let sf = SourceFile::new(&PathBuf::from("<test>"), String::from(""));
  let tree = ErlAstTree::new(Arc::new(sf), Rc::new(ErlAst::Empty));

  tree.any_to_ast(parse_output)
}
