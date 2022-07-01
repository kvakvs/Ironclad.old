//! Function type/spec parsing

use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{dash_atom, tok, tok_atom, tok_semicolon};
use crate::erl_syntax::parsers::parse_type;
use crate::erl_syntax::parsers::parser_error::ErlParserError;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::preprocessor::pp_node::pp_impl::PreprocessorNodeImpl;
use crate::erl_syntax::preprocessor::pp_node::PreprocessorNode;
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::source_loc::SourceLoc;
use crate::typing::erl_type::ErlTypeImpl;
use crate::typing::fn_clause_type::FnClauseType;
use crate::typing::typevar::Typevar;
use libironclad_util::mfarity::MFArity;
use nom::branch::alt;
use nom::combinator::{cut, map, opt};
use nom::error::context;
use nom::multi::separated_list1;
use nom::sequence::{delimited, tuple};

/// Given function spec module attribute `-spec name(args...) -> ...` parse into an AST node
/// Dash `-` is matched outside by the caller.
pub fn parse_fn_spec(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(
    // all between -spec and .
    delimited(
      |i1| dash_atom(i1, "spec"),
      tuple((
        context("function name in a -spec() attribute", cut(tok_atom)),
        separated_list1(
          tok_semicolon,
          context("function clause in a -spec() attribute", cut(parse_fn_spec_fnclause)),
        ),
      )),
      tok(TokenType::Period), // TODO: Check for newline smh? Newline token?
    ),
    |(name, clauses)| {
      let arity = clauses[0].arity();
      assert!(
        clauses.iter().all(|c| c.arity() == arity),
        "All function clauses must have same arity in a typespec"
      );
      let funarity = MFArity::new_local(&name, arity);
      let fntypespec = ErlTypeImpl::new_fn_type(&clauses);
      PreprocessorNodeImpl::new_fn_spec(SourceLoc::new(&input), funarity, fntypespec.into())
    },
  )(input.clone())
}

/// Parses a function clause args specs, return spec and optional `when`
fn parse_fn_spec_fnclause(
  input: ParserInput,
) -> nom::IResult<ParserInput, FnClauseType, ErlParserError> {
  map(
    tuple((
      // Function clause name
      opt(tok_atom),
      // Args list (list of type variables with some types possibly)
      context(
        "arguments list in a function clause spec",
        cut(parse_type::parse_parenthesized_arg_spec_list),
      ),
      tok(TokenType::RightArr),
      // Return type for fn clause
      context(
        "return type in function clause spec",
        cut(alt((
          parse_type::parse_typevar_with_opt_type,
          parse_type::parse_type_as_typevar,
        ))),
      ),
      // Optional: when <comma separated list of typevariables given types>
      context("when expression for typespec", opt(parse_type::parse_when_expr_for_type)),
    )),
    |(_name, args, _arrow, ret_ty, when_expr)| {
      // TODO: Check name equals function name, for module level functions
      if let Some(when_expr_val) = when_expr {
        FnClauseType::new(
          Typevar::merge_lists(&args, &when_expr_val),
          Typevar::substitute_var_from_when_clause(&ret_ty, &when_expr_val).clone(),
        )
      } else {
        FnClauseType::new(args, ret_ty)
      }
    },
  )(input)
}
