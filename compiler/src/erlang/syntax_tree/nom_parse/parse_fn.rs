//! Parse function definitions with Nom

use std::sync::Arc;

use nom::{combinator, sequence, multi, character,
          bytes::complete::{tag}};

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::node::erl_fn_clause::ErlFnClause;
use crate::erlang::syntax_tree::node::erl_fn_def::ErlFnDef;
use crate::erlang::syntax_tree::nom_parse::{misc, parse_atom, parse_expr};
use crate::mfarity::MFArity;
use crate::source_loc::SourceLoc;

fn parse_when_expr(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  combinator::map(
    sequence::tuple((
      misc::ws_before(tag("when")),
      parse_expr::parse_expr,
    )),
    |(_, g)| g, // ignore 'when' tag, keep guard expr
  )(input)
}

/// Parses a named clause for a top level function
fn parse_fnclause(input: &str) -> nom::IResult<&str, ErlFnClause> {
  combinator::map(
    sequence::tuple((
      // Function clause name
      misc::ws_before_mut(combinator::opt(parse_atom::atom)),

      // Args list
      parse_expr::parse_parenthesized_list,

      // Optional: when <guard>
      combinator::opt(parse_when_expr),
      misc::ws_before(tag("->")),

      // Body
      parse_expr::parse_expr,
    )),
    |(name, args, when_expr, _arrow, body)| {
      ErlFnClause::new(name, args, body, when_expr)
    },
  )(input)
}

/// Builds a function definition from multiple parsed clauses
fn build_fndef_fn(fnclauses: Vec<ErlFnClause>) -> Arc<ErlAst> {
  assert!(!fnclauses.is_empty(), "Function clauses list can't be empty, i don't even..."); // unreachable

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
pub fn parse_fndef(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  combinator::map(
    sequence::terminated(
      multi::separated_list1(
        misc::ws_before(character::complete::char(';')),
        parse_fnclause,
      ),
      misc::ws_before(character::complete::char('.')),
    ),
    build_fndef_fn,
  )(input)
}

/// Lambda is an inline function definition
pub fn parse_lambda(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  // Lambda is made of "fun" keyword, followed by multiple ";" separated clauses
  combinator::map(
    sequence::tuple((
      misc::ws_before(tag("fun")),
      multi::separated_list1(
        misc::ws_before(character::complete::char(';')),
        misc::ws_before(parse_fnclause),
      )
    )), |(_, fnclauses)| build_fndef_fn(fnclauses),
  )(input)
}
