use crate::syntaxtree::erl::erl_parser::{ErlParser, Rule};
use crate::syntaxtree::erl::erl_ast::{ErlAst, ErlAstTree};
use pest::iterators::{Pair};
use pest::Parser;
use std::sync::Arc;
use crate::project::source_file::SourceFile;
use crate::erl_error::ErlResult;

/// Parses Erlang syntax of .ERL/.HRL files or arbitrary string input
impl ErlAstTree {
  pub fn from_source_file(source_file: &Arc<SourceFile>) -> ErlResult<ErlAstTree> {
    let successful_parse = ErlParser::parse(Rule::forms, &source_file.text)?.next().unwrap();

    let mut erl_tree = ErlAstTree {
      source: source_file.clone(),
      nodes: vec![],
    };

    match erl_tree.erl_parse_tokens_to_ast(successful_parse) {
      Ok(ErlAst::Forms(nodes)) => {
        // pp_tree.nodes.iter().for_each(|n| println!("Node: {:?}", n));
        erl_tree.nodes = nodes;
        Ok(erl_tree)
      }
      _ => panic!("Only 'file' AST node is expected as erl_ast_tree parse result root")
    }
  }

  // Convert Pest syntax token tree produced by the Pest PEG parser into Erlang AST tree
  pub fn erl_parse_tokens_to_ast(&self, pair: Pair<Rule>) -> ErlResult<ErlAst> {
    let result = match pair.as_rule() {
      Rule::forms => {
        // Parse all nested file elements, comments and text fragments
        let ast_nodes = pair.into_inner()
            .map(|p| self.erl_parse_tokens_to_ast(p))
            .map(|r| r.unwrap())
            .collect::<Vec<ErlAst>>();
        ErlAst::Forms(ast_nodes)
      },
      Rule::string => {
        ErlAst::String(String::from(pair.as_str()))
      }

      other => unreachable!("ErlAst value: {:?}", other),
    };
    Ok(result)
  }
}