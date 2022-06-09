//! Parse function definitions with Nom

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_fn_clause::ErlFnClause;
use crate::erl_syntax::parsers::defs::ParserInput;
use crate::erl_syntax::parsers::defs::{ErlParserError, ParserResult};
use crate::erl_syntax::parsers::misc::{
  match_word, period_tag, semicolon_tag, ws_before, ws_before_mut,
};
use crate::erl_syntax::parsers::parse_expr::{
  parse_comma_sep_exprs1, parse_expr, parse_parenthesized_list_of_exprs, EXPR_STYLE_FULL,
  EXPR_STYLE_MATCHEXPR,
};
use crate::erl_syntax::parsers::parse_strings::atom_literal::parse_atom;
use crate::source_loc::SourceLoc;
use libironclad_util::mfarity::MFArity;
use nom::character::complete::char;
use nom::combinator::{cut, map, not, opt, peek};
use nom::multi::separated_list1;
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::{bytes::complete::tag, error::context};

fn parse_when_expr_for_fn(input: ParserInput) -> ParserResult<AstNode> {
  map(
    tuple((ws_before(tag("when".into())), cut(parse_expr))),
    |(_, g)| g, // ignore 'when' tag, keep guard expr
  )(input)
}

/// Function will succeed if an atom is parsed and FN_NAME is true, will return Some<Value>.
/// Function will succeed if no atom found but also FN_NAME is false, will return None.
/// Function will fail otherwise.
fn parse_fnclause_name<const REQUIRE_FN_NAME: bool>(
  input: ParserInput,
) -> ParserResult<Option<String>> {
  if REQUIRE_FN_NAME {
    // Succeed if FN_NAME=true and there is an atom
    return context("function clause name", map(parse_atom, Some))(input);
  }
  // Succeed if FN_NAME=false and there is no atom
  context(
    "function clause without a name (must not begin with an atom)",
    map(peek(not(parse_atom)), |_| None),
  )(input)
}

/// Parses a named clause for a top level function
/// * FN_NAME: true if the parser must require function name
fn parse_fnclause<const REQUIRE_FN_NAME: bool>(
  input: ParserInput,
) -> nom::IResult<ParserInput, ErlFnClause, ErlParserError> {
  map(
    tuple((
      // Function clause name
      ws_before_mut(parse_fnclause_name::<REQUIRE_FN_NAME>),
      // Function arguments
      context(
        "function clause arguments of a function definition",
        parse_parenthesized_list_of_exprs::<{ EXPR_STYLE_MATCHEXPR }>,
      ),
      // Optional: when <guard>
      context("`when` expression of a function clause", opt(parse_when_expr_for_fn)),
      preceded(
        ws_before(tag("->".into())),
        // Body as list of exprs
        context(
          "function clause body of a function definition",
          cut(parse_comma_sep_exprs1::<{ EXPR_STYLE_FULL }>),
        ),
      ),
    )),
    |(maybe_name, args, when_expr, body)| {
      ErlFnClause::new(maybe_name, args, AstNodeImpl::new_comma_expr(input.loc(), body), when_expr)
    },
  )(input.clone())
}

/// Builds a function definition from multiple parsed clauses
fn _construct_fndef(location: SourceLoc, fnclauses: Vec<ErlFnClause>) -> AstNode {
  // unreachable
  assert!(!fnclauses.is_empty(), "Function clauses list can't be empty, i don't even...");
  // println!("Construct fdef: {:?}", fnclauses);

  let arity = fnclauses[0].args.len();
  let fn_name = match &fnclauses[0].name {
    None => "TODO: lambda_name".to_string(),
    Some(s) => s.clone(),
  };
  let funarity = MFArity::new_local(&fn_name, arity);

  if !fnclauses.iter().all(|fnc| fnc.args.len() == arity) {
    panic!("Not all clauses have same arity")
  }

  AstNodeImpl::new_fndef(location, funarity, fnclauses)
}

/// Parse function definition
pub fn parse_fndef(input: ParserInput) -> ParserResult<AstNode> {
  // print_input("parse_fndef", input);
  map(
    delimited(
      // does not begin with - (that would be a mis-parsed attribute)
      not(peek(ws_before(char('-')))),
      separated_list1(
        semicolon_tag,
        // if parse fails under here, will show this context message in error
        context("function clause of a function definition", parse_fnclause::<true>),
      ),
      period_tag,
    ),
    |t| _construct_fndef(input.loc(), t),
  )(input.clone())
}

/// Lambda is an inline function definition
pub fn parse_lambda(input: ParserInput) -> ParserResult<AstNode> {
  // Lambda is made of "fun" keyword, followed by multiple ";" separated clauses
  map(
    preceded(
      match_word("fun".into()),
      terminated(
        context("", separated_list1(semicolon_tag, parse_fnclause::<false>)),
        match_word("end".into()),
      ),
    ),
    |t| _construct_fndef(input.loc(), t),
  )(input.clone())
}
