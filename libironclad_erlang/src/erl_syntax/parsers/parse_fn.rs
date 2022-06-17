//! Parse function definitions with Nom

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_fn_clause::ErlFnClause;
use crate::erl_syntax::parsers::defs::{ErlParserError, ParserResult};
use crate::erl_syntax::parsers::misc::{tok, tok_atom, tok_keyword};
use crate::erl_syntax::parsers::parse_expr::{
  parse_comma_sep_exprs1, parse_guardexpr, parse_parenthesized_list_of_exprs, EXPR_STYLE_FULL,
  EXPR_STYLE_MATCHEXPR,
};
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::token_stream::keyword::Keyword;
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::source_loc::SourceLoc;
use libironclad_util::mfarity::MFArity;
use nom::combinator::{cut, map, not, opt, peek};
use nom::error::context;
use nom::multi::separated_list1;
use nom::sequence::{delimited, preceded, terminated, tuple};

/// Function will succeed if an atom is parsed and FN_NAME is true, will return Some<Value>.
/// Function will succeed if no atom found but also FN_NAME is false, will return None.
/// Function will fail otherwise.
fn parse_fnclause_name<const REQUIRE_FN_NAME: bool>(
  input: ParserInput,
) -> ParserResult<Option<String>> {
  if REQUIRE_FN_NAME {
    // Succeed if FN_NAME=true and there is an atom
    return context("function clause name", map(tok_atom, Some))(input);
  }
  // Succeed if FN_NAME=false and there is no atom
  context(
    "function clause without a name (must not begin with an atom)",
    map(peek(not(tok_atom)), |_| None),
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
      parse_fnclause_name::<REQUIRE_FN_NAME>,
      // Function arguments
      context(
        "function clause arguments of a function definition",
        parse_parenthesized_list_of_exprs::<{ EXPR_STYLE_MATCHEXPR }>,
      ),
      // Optional: when <guard>
      context(
        "`when` expression of a function clause",
        opt(preceded(
          tok_keyword(Keyword::When),
          context("function's guard", cut(parse_guardexpr)),
        )),
      ),
      preceded(
        tok(TokenType::RightArr),
        // Body as list of exprs
        context(
          "function clause body of a function definition",
          cut(parse_comma_sep_exprs1::<{ EXPR_STYLE_FULL }>),
        ),
      ),
    )),
    |(maybe_name, args, when_expr, body)| {
      ErlFnClause::new(
        maybe_name,
        args,
        AstNodeImpl::new_comma_expr(SourceLoc::new(&input), body),
        when_expr,
      )
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
      not(peek(tok(TokenType::Minus))),
      separated_list1(
        tok(TokenType::Semicolon),
        // if parse fails under here, will show this context message in error
        context("function clause of a function definition", parse_fnclause::<true>),
      ),
      tok(TokenType::Period),
    ),
    |t| _construct_fndef(SourceLoc::new(&input), t),
  )(input.clone())
}

/// Lambda is an inline function definition
pub(crate) fn parse_lambda(input: ParserInput) -> ParserResult<AstNode> {
  // Lambda is made of "fun" keyword, followed by multiple ";" separated clauses
  map(
    preceded(
      tok_keyword(Keyword::Fun),
      terminated(
        context("", separated_list1(tok(TokenType::Semicolon), parse_fnclause::<false>)),
        tok_keyword(Keyword::End),
      ),
    ),
    |t| _construct_fndef(SourceLoc::new(&input), t),
  )(input.clone())
}
