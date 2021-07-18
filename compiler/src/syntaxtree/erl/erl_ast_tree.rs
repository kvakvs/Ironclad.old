//! Defines additional operations on Erlang syntax tree
use crate::syntaxtree::erl::erl_parser::{ErlParser, Rule};
use crate::syntaxtree::erl::erl_ast::{ErlAst, ErlAstTree};
use pest::iterators::{Pair};
use pest::Parser;
use std::sync::Arc;
use crate::project::source_file::SourceFile;
use crate::erl_error::{ErlResult};
use crate::syntaxtree::erl::literal::ErlLit;
use std::rc::Rc;
use std::path::PathBuf;

impl ErlAstTree {
  /// Parses Erlang syntax of .ERL/.HRL files or arbitrary string input
  pub fn from_source_file(source_file: &Arc<SourceFile>) -> ErlResult<ErlAstTree> {
    let successful_parse = ErlParser::parse(Rule::forms, &source_file.text)?.next().unwrap();
    // println!("Parse tokens: {:?}", successful_parse);

    let mut erl_tree = ErlAstTree {
      source: source_file.clone(),
      nodes: Rc::new(ErlAst::Empty),
    };

    match erl_tree.any_to_ast(successful_parse) {
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

  /// Convert Pest syntax token tree produced by the Pest PEG parser into Erlang AST tree
  pub fn any_to_ast(&self, pair: Pair<Rule>) -> ErlResult<Rc<ErlAst>> {
    let pair_s = pair.as_str(); // use for reporting erroneous text

    let result: Rc<ErlAst> = match pair.as_rule() {
      Rule::forms => self.file_root_to_ast(pair)?,
      Rule::string => {
        let s = ErlAst::Lit(ErlLit::String(String::from(pair.as_str())));
        Rc::new(s)
      }
      Rule::module_attr => {
        let ma = ErlAst::ModuleAttr { name: String::from(pair.into_inner().as_str()) };
        Rc::new(ma)
      }
      Rule::function_def => self.function_def_to_ast(pair)?,
      Rule::expr => self.expr_to_ast(pair)?,
      Rule::capitalized_ident => ErlAst::new_var(pair.as_str()),
      //Rule::literal => self.literal_to_ast(pair)?,
      Rule::number_int => {
        let val = pair_s.parse::<isize>()?;
        ErlAst::new_lit_int(val)
      }

      other => todo!("process ErlAst value: {:?}", other),
    };
    Ok(result)
  }

  /// Parse all nested file elements, comments and text fragments
  fn file_root_to_ast(&self, pair: Pair<Rule>) -> ErlResult<Rc<ErlAst>> {
    assert_eq!(pair.as_rule(), Rule::forms);
    let ast_nodes = pair.into_inner()
        .map(|p| self.any_to_ast(p))
        .map(Result::unwrap)
        .collect();
    Ok(Rc::new(ErlAst::Forms(ast_nodes)))
  }

  /// Parse funname(arg, arg...) -> body.
  fn function_def_to_ast(&self, pair: Pair<Rule>) -> ErlResult<Rc<ErlAst>> {
    let pair_s = pair.as_str();
    println!("Parsing: function_def from {}", pair_s);

    assert_eq!(pair.as_rule(), Rule::function_def);

    let p = pair.into_inner();
    let name = p.as_str();
    let clauses: Vec<Rc<ErlAst>> =
        p.map(|p| self.fun_clause_to_ast(p))
            .map(Result::unwrap)
            .collect();
    Ok(ErlAst::new_fun(name, clauses))
  }

  fn fun_clause_to_ast(&self, pair: Pair<Rule>) -> ErlResult<Rc<ErlAst>> {
    assert_eq!(pair.as_rule(), Rule::function_clause);
    let pair_s = pair.as_str();
    let nodes: Vec<Rc<ErlAst>> = pair.into_inner()
        .map(|p| self.any_to_ast(p))
        .map(Result::unwrap)
        .collect();
    todo!("Parsing fclause result={:?}\nfrom: {:?}", nodes, pair_s);
    // Ok(ErlAst::new_fclause(nodes, Rc::new(ErlAst::Empty)))
  }

  /// Parse a generic comma separated list of expressions, if more than one element is found, wrap
  /// them into a Comma AST node, otherwise return as is.
  fn expr_to_ast(&self, pair: Pair<Rule>) -> ErlResult<Rc<ErlAst>> {
    // let pair_s = pair.as_str();
    let expr_item = pair.into_inner();

    let mut ast_items: Vec<Rc<ErlAst>> = expr_item
        .map(|p| self.any_to_ast(p))
        .map(Result::unwrap)
        .collect();

    if ast_items.len() == 1 {
      Ok(ast_items.pop().unwrap())
    } else {
      Ok(ErlAst::new_comma(ast_items))
    }
  }

  // fn literal_to_ast(&self, pair: Pair<Rule>) -> ErlResult<Rc<ErlAst>> {
  //   let pair_s = pair.as_str();
  //   match pair.as_rule() {
  //     _ => Err(ErlError::ErlangParse {
  //       loc: ErrorLocation::None,
  //       msg: format!("Literal is expected, found: {:?} (in {})", pair.as_rule(), pair_s),
  //     })
  //   }
  // }
}
