//! Parse function definitions with Nom

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_fn_clause::ErlFnClause;
use crate::erl_syntax::parsers::defs::{ErlParserError, ParserInput, ParserResult};
use crate::erl_syntax::parsers::misc::{
  match_word, period, print_input, semicolon, ws_before, ws_before_mut,
};
use crate::erl_syntax::parsers::parse_atom::AtomParser;
use crate::erl_syntax::parsers::ErlParser;
use libironclad_error::source_loc::SourceLoc;
use libironclad_util::mfarity::MFArity;
use nom::character::complete::char;
use nom::combinator::{cut, map, not, opt, peek};
use nom::multi::separated_list1;
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::{bytes::complete::tag, error::context};

impl ErlParser {
  fn parse_when_expr_for_fn(input: ParserInput) -> ParserResult<AstNode> {
    map(
      tuple((ws_before(tag("when")), cut(Self::parse_expr))),
      |(_, g)| g, // ignore 'when' tag, keep guard expr
    )(input)
  }

  /// Function will succeed if an atom is parsed and FN_NAME is true, will return Some<Value>.
  /// Function will succeed if no atom found but also FN_NAME is false, will return None.
  /// Function will fail otherwise.
  fn parse_fnclause_name<const REQUIRE_FN_NAME: bool>(
    input: ParserInput,
  ) -> nom::IResult<&str, Option<String>, ErlParserError> {
    if REQUIRE_FN_NAME {
      // Succeed if FN_NAME=true and there is an atom
      return context("function clause name", cut(map(AtomParser::atom, Some)))(input);
    }
    // Succeed if FN_NAME=false and there is no atom
    context(
      "function clause without a name (must not begin with an atom)",
      cut(map(peek(not(AtomParser::atom)), |_| None)),
    )(input)
  }

  /// Parses a named clause for a top level function
  /// * FN_NAME: true if the parser must require function name
  fn parse_fnclause<const REQUIRE_FN_NAME: bool>(
    input: ParserInput,
  ) -> nom::IResult<&str, ErlFnClause, ErlParserError> {
    map(
      tuple((
        // Function clause name
        ws_before_mut(Self::parse_fnclause_name::<REQUIRE_FN_NAME>),
        // Function arguments
        context(
          "function clause arguments of a function definition",
          Self::parse_parenthesized_list_of_exprs::<{ ErlParser::EXPR_STYLE_MATCHEXPR }>,
        ),
        // Optional: when <guard>
        context("`when` expression of a function clause", opt(Self::parse_when_expr_for_fn)),
        preceded(
          ws_before(tag("->")),
          // Body as list of exprs
          context(
            "function clause body of a function definition",
            cut(Self::parse_comma_sep_exprs1::<{ ErlParser::EXPR_STYLE_FULL }>),
          ),
        ),
      )),
      |(maybe_name, args, when_expr, body)| {
        ErlFnClause::new(
          maybe_name,
          args,
          AstNodeImpl::new_comma_expr(&SourceLoc::from_input(input), body),
          when_expr,
        )
      },
    )(input)
  }

  /// Builds a function definition from multiple parsed clauses
  fn _construct_fndef(location: &SourceLoc, fnclauses: Vec<ErlFnClause>) -> AstNode {
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
    print_input("parse_fndef", input);
    map(
      delimited(
        // does not begin with - (that would be a mis-parsed attribute)
        not(peek(ws_before(char('-')))),
        separated_list1(
          semicolon,
          // if parse fails under here, will show this context message in error
          context("function clause of a function definition", Self::parse_fnclause::<true>),
        ),
        period,
      ),
      |t| Self::_construct_fndef(&SourceLoc::from_input(input), t),
    )(input)
  }

  /// Lambda is an inline function definition
  pub fn parse_lambda(input: ParserInput) -> ParserResult<AstNode> {
    // Lambda is made of "fun" keyword, followed by multiple ";" separated clauses
    map(
      preceded(
        match_word("fun"),
        terminated(
          context("", separated_list1(semicolon, Self::parse_fnclause::<false>)),
          match_word("end"),
        ),
      ),
      |t| Self::_construct_fndef(&SourceLoc::from_input(input), t),
    )(input)
  }
}
