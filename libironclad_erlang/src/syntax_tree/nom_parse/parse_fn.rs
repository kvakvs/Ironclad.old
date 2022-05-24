//! Parse function definitions with Nom

use std::sync::Arc;

use crate::syntax_tree::erl_ast::ErlAst;
use crate::syntax_tree::node::erl_fn_clause::ErlFnClause;
use crate::syntax_tree::nom_parse::misc::{ws_before, ws_before_mut};
use crate::syntax_tree::nom_parse::parse_atom::AtomParser;
use crate::syntax_tree::nom_parse::{AstParserResult, ErlParser, ErlParserError};
use libironclad_error::source_loc::SourceLoc;
use libironclad_util::mfarity::MFArity;
use nom::combinator::{cut, map, opt};
use nom::multi::separated_list1;
use nom::sequence::{preceded, terminated, tuple};
use nom::{bytes::complete::tag, character::complete::char, combinator, error::context};

impl ErlParser {
  fn parse_when_expr_for_fn(input: &str) -> AstParserResult {
    map(
      tuple((ws_before(tag("when")), cut(Self::parse_expr))),
      |(_, g)| g, // ignore 'when' tag, keep guard expr
    )(input)
  }

  /// Function will succeed if an atom is parsed and FN_NAME is true, will return Some<Value>.
  /// Function will succeed if no atom found but also FN_NAME is false, will return None.
  /// Function will fail otherwise.
  fn parse_fnclause_name<const REQUIRE_FN_NAME: bool>(
    input: &str,
  ) -> nom::IResult<&str, Option<String>, ErlParserError> {
    if REQUIRE_FN_NAME {
      // Succeed if FN_NAME=true and there is an atom
      return map(AtomParser::parse_atom, Option::Some)(input);
    }
    // Succeed if FN_NAME=false and there is no atom
    map(combinator::peek(combinator::not(AtomParser::parse_atom)), |_| Option::None)(input)
  }

  /// Parses a named clause for a top level function
  /// * FN_NAME: true if the parser must require function name
  fn parse_fnclause<const REQUIRE_FN_NAME: bool>(
    input: &str,
  ) -> nom::IResult<&str, ErlFnClause, ErlParserError> {
    map(
      tuple((
        // Function clause name
        ws_before_mut(Self::parse_fnclause_name::<REQUIRE_FN_NAME>),
        // Function arguments
        nom::error::context(
          "function clause arguments",
          Self::parse_parenthesized_list_of_exprs::<{ ErlParser::EXPR_STYLE_MATCHEXPR }>,
        ),
        // Optional: when <guard>
        nom::error::context(
          "when expression in a function clause",
          opt(Self::parse_when_expr_for_fn),
        ),
        nom::error::context(
          "function clause body",
          preceded(
            ws_before(tag("->")),
            // Body as list of exprs
            cut(Self::parse_comma_sep_exprs1::<{ ErlParser::EXPR_STYLE_FULL }>),
          ),
        ),
      )),
      |(maybe_name, args, when_expr, body)| {
        ErlFnClause::new(maybe_name, args, ErlAst::new_comma_expr(SourceLoc::None, body), when_expr)
      },
    )(input)
  }

  /// Builds a function definition from multiple parsed clauses
  fn _construct_fndef(fnclauses: Vec<ErlFnClause>) -> Arc<ErlAst> {
    assert!(!fnclauses.is_empty(), "Function clauses list can't be empty, i don't even..."); // unreachable
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

    ErlAst::new_fndef(SourceLoc::None, funarity, fnclauses)
  }

  /// Parse function definition
  pub fn parse_fndef(input: &str) -> AstParserResult {
    map(
      terminated(
        separated_list1(
          ws_before(char(';')),
          // if parse fails under here, will show this context message in error
          context("function clause", cut(Self::parse_fnclause::<true>)),
        ),
        ws_before(char('.')),
      ),
      Self::_construct_fndef,
    )(input)
  }

  /// Lambda is an inline function definition
  pub fn parse_lambda(input: &str) -> AstParserResult {
    // Lambda is made of "fun" keyword, followed by multiple ";" separated clauses
    map(
      preceded(
        ws_before(tag("fun")),
        terminated(
          context("", separated_list1(ws_before(char(';')), Self::parse_fnclause::<false>)),
          ws_before(tag("end")),
        ),
      ),
      Self::_construct_fndef,
    )(input)
  }
}
