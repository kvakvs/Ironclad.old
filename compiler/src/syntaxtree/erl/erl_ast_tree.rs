//! Defines additional operations on Erlang syntax tree
use crate::syntaxtree::erl::erl_parser::{ErlParser, Rule};
use crate::syntaxtree::erl::erl_ast::{ErlAst, ErlAstTree, ErlToken};
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
      Rule::expr => self.expr_to_ast(self.parse_inner(pair)?)?,
      Rule::bindable_expr => self.bindable_expr_to_ast(self.parse_inner(pair)?)?,
      Rule::capitalized_ident => ErlAst::new_var(pair.as_str()),
      Rule::number_int => {
        let val = pair_s.parse::<isize>()?;
        ErlAst::new_lit_int(val)
      }
      Rule::atom => ErlAst::new_lit_atom(pair.as_str()),

      // Temporary tokens must be consumed by this function and never exposed to the
      // rest of the program
      Rule::op_plus => ErlAst::temporary_token(ErlToken::Plus),
      Rule::op_minus => ErlAst::temporary_token(ErlToken::Minus),
      Rule::op_not => ErlAst::temporary_token(ErlToken::Not),
      Rule::op_bnot => ErlAst::temporary_token(ErlToken::BinaryNot),
      Rule::op_div => ErlAst::temporary_token(ErlToken::Div),
      Rule::op_mul => ErlAst::temporary_token(ErlToken::Mul),
      Rule::op_integer_div => ErlAst::temporary_token(ErlToken::IntegerDiv),
      Rule::op_remainder => ErlAst::temporary_token(ErlToken::Remainder),
      Rule::op_band => ErlAst::temporary_token(ErlToken::BinaryAnd),
      Rule::op_and => ErlAst::temporary_token(ErlToken::And),
      Rule::op_bor => ErlAst::temporary_token(ErlToken::BinaryOr),
      Rule::op_bxor => ErlAst::temporary_token(ErlToken::BinaryXor),
      Rule::op_bsl => ErlAst::temporary_token(ErlToken::BinaryShiftLeft),
      Rule::op_bsr => ErlAst::temporary_token(ErlToken::BinaryShiftRight),
      Rule::op_or => ErlAst::temporary_token(ErlToken::Or),
      Rule::op_xor => ErlAst::temporary_token(ErlToken::Xor),
      Rule::op_list_append => ErlAst::temporary_token(ErlToken::ListAppend),
      Rule::op_list_subtract => ErlAst::temporary_token(ErlToken::ListSubtract),
      Rule::op_eq => ErlAst::temporary_token(ErlToken::Eq),
      Rule::op_neq => ErlAst::temporary_token(ErlToken::NotEq),
      Rule::op_lteq => ErlAst::temporary_token(ErlToken::LessThanEq),
      Rule::op_lt => ErlAst::temporary_token(ErlToken::LessThan),
      Rule::op_geq => ErlAst::temporary_token(ErlToken::GreaterEq),
      Rule::op_gt => ErlAst::temporary_token(ErlToken::GreaterThan),
      Rule::op_hard_eq => ErlAst::temporary_token(ErlToken::HardEq),
      Rule::op_hard_neq => ErlAst::temporary_token(ErlToken::HardNotEq),
      Rule::op_andalso => ErlAst::temporary_token(ErlToken::AndAlso),
      Rule::op_orelse => ErlAst::temporary_token(ErlToken::OrElse),
      Rule::op_assign => ErlAst::temporary_token(ErlToken::Assign),
      Rule::op_send => ErlAst::temporary_token(ErlToken::Send),
      Rule::op_catch => ErlAst::temporary_token(ErlToken::Catch),
      Rule::op_comma => ErlAst::temporary_token(ErlToken::Comma),

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
    // let pair_s = pair.as_str();
    assert_eq!(pair.as_rule(), Rule::function_def);

    // Step down twice to get the function name, and then will be followed by
    // args, optional guard and expr body
    // let mut p = pair.into_inner().next().unwrap().into_inner();
    // let name = p.next().unwrap().as_str();

    let clauses: Vec<Rc<ErlAst>> =
        pair.into_inner()
            .map(|p| self.fun_clause_to_ast(p))
            .map(Result::unwrap)
            .collect();
    println!("Parsing: function_def {:?}", clauses );
    Ok(ErlAst::new_fun(clauses))
  }

  fn fun_clause_to_ast(&self, pair: Pair<Rule>) -> ErlResult<Rc<ErlAst>> {
    // assert_eq!(pair.as_rule(), Rule::function_clause);

    // let pair_s = pair.as_str();
    let nodes: Vec<Rc<ErlAst>> = pair.into_inner()
        .map(|p| self.any_to_ast(p))
        .map(Result::unwrap)
        .collect();

    // First node of a fun clause is the name
    // Last node of a function clause is the expression body
    // Args are in between
    assert!(nodes.len() > 2, "Nodes must include function name and at least 1 expr");

    let name = &nodes[0].get_atom_text().unwrap();
    let args = nodes[1..nodes.len()-1].iter()
        .cloned()
        .collect();
    let body = {
      let body_nodes = nodes[nodes.len() - 1].clone();
      self.expr_to_ast(body_nodes)?.clone()
    };
    Ok(ErlAst::new_fclause(name, args, body))
  }

  /// Parses all inner nodes to produce a stream of AST nodes, the caller is expected to make sense
  /// of the nodes and verify they're correct.
  fn parse_inner(&self, pair: Pair<Rule>) -> ErlResult<Rc<ErlAst>> {
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

  /// Parse a generic comma separated list of expressions, if more than one element is found, wrap
  /// them into a Comma AST node, otherwise return as is.
  fn expr_to_ast(&self, ast: Rc<ErlAst>) -> ErlResult<Rc<ErlAst>> {
    Ok(ast)
  }

  /// Parse a generic bindable expression, this is used as function arguments, in matching and
  /// variable assignments.
  fn bindable_expr_to_ast(&self, ast: Rc<ErlAst>) -> ErlResult<Rc<ErlAst>> {
    Ok(ast)
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
