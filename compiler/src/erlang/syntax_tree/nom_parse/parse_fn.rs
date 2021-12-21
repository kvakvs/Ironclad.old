//! Parse function definitions with Nom

use std::sync::Arc;

use nom::{combinator, sequence, multi, branch,
          bytes::complete::{tag}};

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::node::erl_fn_clause::ErlFnClause;
use crate::erlang::syntax_tree::node::erl_fn_def::ErlFnDef;
use crate::erlang::syntax_tree::nom_parse::{misc, parse_atom};
use crate::erlang::syntax_tree::nom_parse::parse_expr::parse_guard_expr;
use crate::mfarity::MFArity;
use crate::source_loc::SourceLoc;

fn parse_when_expr(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  sequence::tuple((
    misc::ws(tag("when")),
    parse_guard_expr,
  ))(input)
}

fn parse_bindable_expr(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {}

fn parse_fnclause(input: &str) -> nom::IResult<&str, ErlFnClause> {
  sequence::tuple((
    parse_atom::atom,

    // Args list: many bindable expressions, delimited by parentheses, separated by commas
    sequence::delimited(
      misc::ws(tag("(")),
      multi::separated_list0(misc::ws(tag(",")), parse_bindable_expr),
      misc::ws(tag(")"))),
    combinator::opt(parse_when_expr)
  ))(input)
}

/// Parse function definition
pub fn parse_fndef(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  let build_fndef_fn = |fnclauses| {
    assert!(!fnclauses.is_empty(), "Function clauses list can't be empty, i don't even..."); // unreachable

    let arity = fnclauses[0].args.len();
    let funarity = MFArity::new_local(&fnclauses[0].name, arity);

    if !fnclauses.iter().all(|fnc| fnc.args.len() == arity) {
      panic!("Not all clauses have same arity")
    }

    let fndef = ErlFnDef {
      location: SourceLoc::None,
      funarity,
      clauses: fnclauses,
    };
    ErlAst::FnDef(fndef).into()
  };

  combinator::map(multi::separated_list1(
    tag(";"),
    parse_fnclause,
  ), build_fndef_fn,
  )(input)
}
