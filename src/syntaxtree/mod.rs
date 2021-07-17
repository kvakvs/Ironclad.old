pub mod pp;
pub mod erl;

pub mod ast_cache;

#[cfg(test)]
mod test_util {
  use crate::syntaxtree::pp::pp_parser;
  use crate::syntaxtree::erl::erl_parser;
  use crate::erl_error::ErlResult;
  use crate::syntaxtree::pp::pp_ast::{PpAst, PpAstTree};
  use crate::project::source_file::SourceFile;
  use std::sync::Arc;
  use crate::syntaxtree::erl::erl_ast::{ErlAst, ErlAstTree};
  use std::path::PathBuf;
  use pest::Parser;
  use std::rc::Rc;

  pub fn pp_parse(rule: pp_parser::Rule, input: &str) -> ErlResult<Rc<PpAst>> {
    let parse_output = pp_parser::PpParser::parse(rule, input)?.next().unwrap();

    let sf = SourceFile::new(&PathBuf::from("<test>"), String::from(""));
    let tree = PpAstTree::new(Arc::new(sf), Rc::new(PpAst::Empty));

    tree.pp_parse_tokens_to_ast(parse_output)
  }

  pub fn erl_parse(rule: erl_parser::Rule, input: &str) -> ErlResult<Rc<ErlAst>> {
    let parse_output = erl_parser::ErlParser::parse(rule, input)?.next().unwrap();

    let sf = SourceFile::new(&PathBuf::from("<test>"), String::from(""));
    let tree = ErlAstTree::new(Arc::new(sf), Rc::new(ErlAst::Empty));

    tree.erl_parse_tokens_to_ast(parse_output)
  }
}