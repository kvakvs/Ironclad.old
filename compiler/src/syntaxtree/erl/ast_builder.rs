//! Processes parser output and produces ErlAst tree for the program
use crate::erl_error::{ErlResult};
use crate::syntaxtree::erl::erl_ast::{ErlAst, ErlToken};
use crate::syntaxtree::erl::erl_op::ErlBinaryOp;
use crate::syntaxtree::erl::erl_parser::{Rule, get_prec_climber};
use crate::syntaxtree::erl::node::fun_clause_node::FunctionClauseNode;
use crate::syntaxtree::erl::node::literal_node::LiteralNode;
use pest::iterators::{Pair};
use pest::prec_climber::PrecClimber;
use std::rc::Rc;

use crate::erl_module::ErlModule;
use crate::source_loc::SourceLoc;
use std::collections::VecDeque;

impl ErlModule {
  fn prec_climb_infix_fn(lhs0: ErlResult<ErlAst>,
                         op: Pair<Rule>,
                         rhs0: ErlResult<ErlAst>) -> ErlResult<ErlAst> {
    let lhs = lhs0?;
    let rhs = rhs0?;

    // println!("PrecC: infix({:?} ⋄ {} ⋄ {:?}) ->", lhs, op, rhs);
    let loc: SourceLoc = op.as_span().into();
    let result: ErlAst = match op.as_rule() {
      Rule::op_plus => ErlAst::new_binop(loc, lhs, ErlBinaryOp::Add, rhs),
      Rule::op_minus => ErlAst::new_binop(loc, lhs, ErlBinaryOp::Sub, rhs),
      Rule::op_mul => ErlAst::new_binop(loc, lhs, ErlBinaryOp::Mul, rhs),
      Rule::op_div => ErlAst::new_binop(loc, lhs, ErlBinaryOp::Div, rhs),
      Rule::op_integer_div => ErlAst::new_binop(loc, lhs, ErlBinaryOp::IntegerDiv, rhs),
      Rule::op_comma => ErlAst::new_comma(loc, lhs, rhs),
      _ => todo!("any_to_ast_prec_climber: Climber doesn't know how to parse: {:?}", op)
    };
    // println!("PrecC: infix -> result {:?}", &result);
    Ok(result)
  }

  /// Convert Pest syntax token tree produced by the Pest PEG parser into Erlang AST tree
  /// This time using Precedence Climber, processes children of a node.
  /// This should only be used for Rule::expr subtrees where operator precedence makes sense.
  pub fn to_ast_prec_climb(&self, pair: Pair<Rule>, climber: &PrecClimber<Rule>) -> ErlResult<ErlAst> {
    // println!("PrecC: in {}", pair.as_str());

    match pair.as_rule() {
      Rule::expr => {
        let ast_items = climber.climb(
          pair.into_inner(),
          |p| self.to_ast_prec_climb(p, climber),
          Self::prec_climb_infix_fn,
        )?;
        // println!("Climber parsed: {:?}", ast_items);
        Ok(ast_items)
      }
      Rule::literal | Rule::var | Rule::application => {
        self.to_ast_single_node(pair)
      }
      _other => unreachable!("Climber doesn't know how to handle {} (type {:?})", pair.as_str(), pair.as_rule())
    }
  }

  /// Convert Pest syntax token tree produced by the Pest PEG parser into Erlang AST tree.
  /// Processes a single node where there's no need to use precedence climber.
  pub fn to_ast_single_node(&self, pair: Pair<Rule>) -> ErlResult<ErlAst> {
    let loc: SourceLoc = pair.as_span().into();

    let result: ErlAst = match pair.as_rule() {
      Rule::module => self.file_root_to_ast(pair)?,
      Rule::string => {
        ErlAst::Lit(loc, LiteralNode::String(String::from(pair.as_str())))
      }
      Rule::module_attr => {
        ErlAst::ModuleAttr {
          location: loc,
          name: String::from(pair.into_inner().as_str()),
        }
      }
      Rule::function_def => self.function_def_to_ast(pair)?,
      Rule::expr => self.to_ast_prec_climb(pair, get_prec_climber())?,
      Rule::bindable_expr => {
        // self.bindable_expr_to_ast(self.parse_inner(pair)?)?
        self.to_ast_prec_climb(pair.into_inner().next().unwrap(), get_prec_climber())?
        // TODO: Use prec_climber but a different grammar rule
      }
      Rule::capitalized_ident => ErlAst::new_var(loc, pair.as_str()),

      Rule::literal => self.parse_literal(pair.into_inner().next().unwrap())?,
      // moved to parse_literal
      // Rule::number_int => {
      //   let val = pair_s.parse::<isize>()?;
      //   ErlAst::new_lit_int(val)
      // }
      Rule::atom => ErlAst::new_lit_atom(loc, pair.as_str()),
      Rule::var => ErlAst::new_var(loc, pair.as_str()),
      Rule::application => self.application_to_ast(pair)?,

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

      Rule::COMMENT => ErlAst::Comment(pair.as_span().into()),
      other => todo!("to_ast_single_node: unknown parse node {:?}", other),
    };
    Ok(result)
  }

  /// Parsed Application tokens are converted to App AST node.
  /// The expression is analyzed if it resembles anything callable, and is then transformed to a
  /// function pointer (or an export etc).
  fn application_to_ast(&self, pair: Pair<Rule>) -> ErlResult<ErlAst> {
    // println!("application pair {:#?}", &pair);
    let loc: SourceLoc = pair.as_span().into();
    let mut pair_inner = pair.into_inner();

    let expr_node = pair_inner.next().unwrap();
    let expr = self.to_ast_prec_climb(expr_node, get_prec_climber())?;
    // TODO: Attempt to convert expr node to a different callable AST node, i.e. from atom to fun/2 for example. Postprocess after parse?

    let app_node = pair_inner.next().unwrap();

    let result = match app_node.as_rule() {
      Rule::application0 => ErlAst::new_application0(loc, expr),
      Rule::applicationN => {
        // Args come as a single expression (1 arg) or as a Comma expression (multiple)
        // Comma needs to be unrolled into a vector of AST
        let args_node = app_node.into_inner().next().unwrap();
        let args_ast = self.to_ast_prec_climb(args_node, get_prec_climber())?;

        // Unwrap comma operator into a vec of args, a non-comma AST node will become
        // a single vec element
        let mut args = Vec::new();
        ErlAst::comma_to_vec(args_ast, &mut args);

        ErlAst::new_application(loc, expr, args)
      }
      _ => {
        unreachable!("to_ast_single_node: While parsing expr application, can't handle {:?}",
                     app_node.as_rule())
      }
    };
    Ok(result)
  }

  /// Parse all nested file elements, comments and text fragments
  fn file_root_to_ast(&self, pair: Pair<Rule>) -> ErlResult<ErlAst> {
    assert_eq!(pair.as_rule(), Rule::module);
    let ast_nodes = pair.into_inner().into_iter()
        .map(|p| self.to_ast_single_node(p))
        .map(Result::unwrap)
        .collect();
    Ok(ErlAst::ModuleForms(ast_nodes))
  }

  /// Parse funname(arg, arg...) -> body.
  fn function_def_to_ast(&self, pair: Pair<Rule>) -> ErlResult<ErlAst> {
    let location = pair.as_span().into();
    assert_eq!(pair.as_rule(), Rule::function_def);

    let clauses: Vec<FunctionClauseNode> =
        pair.into_inner()
            .map(|p| self.fun_clause_to_ast(p))
            .map(Result::unwrap)
            .collect();
    Ok(ErlAst::new_fun(location, clauses))
  }

  /// Takes a Rule::function_clause and returns ErlAst::FClause
  fn fun_clause_to_ast(&self, pair: Pair<Rule>) -> ErlResult<FunctionClauseNode> {
    assert_eq!(pair.as_rule(), Rule::function_clause);

    // println!("Fun clause {:#?}", pair);

    let mut nodes: VecDeque<ErlAst> = pair.into_inner()
        .map(|p| self.to_ast_single_node(p))
        .map(Result::unwrap)
        .collect();

    // First node of a fun clause is the name
    // Last node of a function clause is the expression body
    // Args are in between
    assert!(nodes.len() >= 2,
            "fun_clause_to_ast: Inner nodes of a function clause must include a function name and at least 1 expr");

    // must always succeed due to assert above
    let name = nodes.pop_front().unwrap().get_atom_text().unwrap();

    let body = {
      // must always succeed due to assert above
      let body_nodes = nodes.pop_back().unwrap();
      self.expr_to_ast(body_nodes)?
    };

    let args: Vec<ErlAst> = nodes.into_iter().collect();
    Ok(FunctionClauseNode::new(name, args, body))
  }

  // /// Parses all inner nodes to produce a stream of AST nodes, the caller is expected to make sense
  // /// of the nodes and verify they're correct.
  // fn parse_inner(&self, pair: Pair<Rule>) -> ErlResult<Rc<ErlAst>> {
  //   // let pair_s = pair.as_str();
  //   let expr_item = pair.into_inner();
  //
  //   let mut ast_items: Vec<Rc<ErlAst>> = expr_item
  //       .map(|p| self.to_ast_single_node(p))
  //       .map(Result::unwrap)
  //       .collect();
  //
  //   if ast_items.len() == 1 {
  //     Ok(ast_items.pop().unwrap())
  //   } else {
  //     Ok(ErlAst::new_comma(ast_items)
  //   }
  // }

  /// Parse a generic comma separated list of expressions, if more than one element is found, wrap
  /// them into a Comma AST node, otherwise return as is.
  fn expr_to_ast(&self, ast: ErlAst) -> ErlResult<ErlAst> {
    Ok(ast)
  }

  /// Parse a generic bindable expression, this is used as function arguments, in matching and
  /// variable assignments.
  fn bindable_expr_to_ast(&self, ast: ErlAst) -> ErlResult<ErlAst> {
    Ok(ast)
  }

  /// Given some token from literal pest parse hierarchy, try and get a literal value out of it
  fn parse_literal(&self, pair: Pair<Rule>) -> ErlResult<ErlAst> {
    let pair_s = pair.as_str(); // use for reporting erroneous text

    let result: ErlAst = match pair.as_rule() {
      Rule::number_int => {
        let val = pair_s.parse::<isize>()?;
        ErlAst::new_lit_int(pair.as_span().into(), val)
      }
      Rule::atom => ErlAst::new_lit_atom(pair.as_span().into(), pair.as_str()),

      other => todo!("parse_literal: unknown parse node {:?}", other),
    };
    Ok(result)
  }
}