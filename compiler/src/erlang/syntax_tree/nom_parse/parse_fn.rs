//! Parse function definitions with Nom

use std::sync::Arc;

use nom::{combinator, sequence, multi,
          character::complete::{char},
          bytes::complete::{tag}};

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::node::erl_fn_clause::ErlFnClause;
use crate::erlang::syntax_tree::node::erl_fn_def::ErlFnDef;
use crate::erlang::syntax_tree::nom_parse::{AstParserResult, ErlParser, ErlParserError};
use crate::erlang::syntax_tree::nom_parse::parse_atom::AtomParser;
use crate::mfarity::MFArity;
use crate::source_loc::SourceLoc;

impl ErlParser {
  fn parse_when_expr_for_fn(input: &str) -> AstParserResult {
    combinator::map(
      sequence::tuple((
        Self::ws_before(tag("when")),
        Self::parse_expr,
      )),
      |(_, g)| g, // ignore 'when' tag, keep guard expr
    )(input)
  }

  /// Parses a named clause for a top level function
  fn parse_fnclause(input: &str) -> nom::IResult<&str, ErlFnClause, ErlParserError> {
    combinator::map(
      sequence::tuple((
        // Function clause name
        Self::ws_before_mut(combinator::opt(AtomParser::parse_atom)),

        // Function arguments
        nom::error::context("function clause arguments",
                            Self::parse_parenthesized_list_of_exprs),

        // Optional: when <guard>
        nom::error::context("when expression",
                            combinator::opt(Self::parse_when_expr_for_fn)),

        sequence::preceded(
          Self::ws_before(tag("->")),
          Self::parse_expr, // Body
        )
      )),
      |(maybe_name, args, when_expr, body)| {
        ErlFnClause::new(maybe_name, args, body, when_expr)
      },
    )(input)
  }

  /// Builds a function definition from multiple parsed clauses
  fn _construct_fndef(fnclauses: Vec<ErlFnClause>) -> Arc<ErlAst> {
    assert!(!fnclauses.is_empty(), "Function clauses list can't be empty, i don't even..."); // unreachable
    println!("Construct fdef: {:?}", fnclauses);

    let arity = fnclauses[0].args.len();
    let fn_name = match &fnclauses[0].name {
      None => "TODO: lambda_name".to_string(),
      Some(s) => s.clone(),
    };
    let funarity = MFArity::new_local(&fn_name, arity);

    if !fnclauses.iter().all(|fnc| fnc.args.len() == arity) {
      panic!("Not all clauses have same arity")
    }

    let fndef = ErlFnDef {
      location: SourceLoc::None,
      funarity,
      clauses: fnclauses,
    };
    ErlAst::FnDef(fndef).into()
  }

  /// Parse function definition
  pub fn parse_fndef(input: &str) -> AstParserResult {
    combinator::map(
      sequence::terminated(
        multi::separated_list1(
          Self::ws_before(char(';')),
          Self::parse_fnclause,
        ),
        Self::ws_before(char('.')),
      ),
      Self::_construct_fndef,
    )(input)
  }

  /// Lambda is an inline function definition
  pub fn parse_lambda(input: &str) -> AstParserResult {
    // Lambda is made of "fun" keyword, followed by multiple ";" separated clauses
    combinator::map(
      sequence::tuple((
        Self::ws_before(tag("fun")),
        multi::separated_list1(
          Self::ws_before(char(';')),
          Self::parse_fnclause,
        )
      )), |(_, fnclauses)| Self::_construct_fndef(fnclauses),
    )(input)
  }
}