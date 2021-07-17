use crate::syntaxtree::erl::erl_parser::{ErlParser, Rule};
use crate::syntaxtree::erl::erl_ast::{ErlAst, ErlAstTree};
use pest::iterators::{Pair};
use pest::Parser;
use std::sync::Arc;
use crate::project::source_file::SourceFile;
use crate::erl_error::ErlResult;
use crate::syntaxtree::erl::literal::ErlLiteral;
use std::rc::Rc;
use std::path::PathBuf;

impl ErlAstTree {
  /// Parses Erlang syntax of .ERL/.HRL files or arbitrary string input
  pub fn from_source_file(source_file: &Arc<SourceFile>) -> ErlResult<ErlAstTree> {
    let successful_parse = ErlParser::parse(Rule::forms, &source_file.text)?.next().unwrap();
    println!("Parse tokens: {:?}", successful_parse);

    let mut erl_tree = ErlAstTree {
      source: source_file.clone(),
      nodes: Rc::new(ErlAst::Empty),
    };

    match erl_tree.erl_parse_tokens_to_ast(successful_parse) {
      Ok(root) => {
        erl_tree.nodes = root;
        Ok(erl_tree)
      }
      _ => panic!("Only 'file' AST node is expected as erl_ast_tree parse result root")
    }
  }

  /// Parses Erlang syntax of .ERL/.HRL files or arbitrary string input
  pub fn from_str(filename: &str, input: &str) -> ErlResult<ErlAstTree> {
    let sf = SourceFile::new(&PathBuf::from(filename), String::from(input));
    ErlAstTree::from_source_file(&Arc::new(sf))
  }

  // Convert Pest syntax token tree produced by the Pest PEG parser into Erlang AST tree
  pub fn erl_parse_tokens_to_ast(&self, pair: Pair<Rule>) -> ErlResult<Rc<ErlAst>> {
    let result: Rc<ErlAst> = match pair.as_rule() {
      Rule::forms => {
        // Parse all nested file elements, comments and text fragments
        let ast_nodes = pair.into_inner()
            .map(|p| self.erl_parse_tokens_to_ast(p))
            .map(Result::unwrap)
            .collect();
        Rc::new(ErlAst::Forms(ast_nodes))
      },
      Rule::string => {
        let s = ErlAst::Lit(ErlLiteral::String(String::from(pair.as_str())));
        Rc::new(s)
      }
      Rule::module_attr => {
        let ma = ErlAst::ModuleAttr { name: String::from(pair.into_inner().as_str()) };
        Rc::new(ma)
      }
      Rule::function_def => {
        let p = pair.into_inner();
        let name = p.as_str();
        let clauses: Vec<Rc<ErlAst>> =
            p.map(|p| self.erl_parse_tokens_to_ast(p))
            .map(Result::unwrap)
            .collect();
        ErlAst::new_fun(name, clauses)
      }

      other => unreachable!("ErlAst value: {:?}", other),
    };
    Ok(result)
  }
}