//! Function type/spec parsing

use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{dash_atom, period_eol_eof, tok_atom};
use crate::erl_syntax::parsers::misc_tok::*;
use crate::erl_syntax::parsers::parse_type;
use crate::erl_syntax::parsers::parse_type::parse_t_util;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::preprocessor::pp_node::pp_impl::PreprocessorNodeImpl;
use crate::erl_syntax::preprocessor::pp_node::PreprocessorNode;
use crate::error::ic_error::IroncladResult;
use crate::source_loc::SourceLoc;
use crate::typing::erl_type::typekind::TypeKind;
use crate::typing::erl_type::{ErlType, TypeImpl};
use crate::typing::fn_clause_type::FnClauseType;
use libironclad_util::mfarity::MFArity;
use nom::combinator::{cut, map, opt};
use nom::error::context;
use nom::multi::separated_list1;
use nom::sequence::{delimited, preceded, tuple};

/// Given function spec module attribute `-spec name(args...) -> ...` parse into an AST node
/// Dash `-` is matched outside by the caller.
pub fn parse_fn_spec(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(
    // all between -spec and .
    delimited(
      |i1| dash_atom(i1, "spec"),
      // TODO: Can be wrapped with parentheses
      tuple((
        context("function name in a -spec() attribute", cut(tok_atom)),
        separated_list1(
          tok_semicolon,
          context("function clause in a -spec() attribute", cut(parse_fn_spec_fnclause)),
        ),
      )),
      period_eol_eof,
    ),
    |(name, clauses)| {
      let arity = clauses[0].arity();
      assert!(
        clauses.iter().all(|c| c.arity() == arity),
        "All function clauses must have same arity in a typespec"
      );
      let funarity = MFArity::new_local(&name, arity);
      let fntypespec_kind = TypeKind::new_fn_type(clauses);
      PreprocessorNodeImpl::new_fn_spec(
        SourceLoc::new(&input),
        funarity,
        TypeImpl::new_unnamed(fntypespec_kind),
      )
    },
  )(input.clone())
}

fn parse_fnclause_map_fn(
  args: Vec<ErlType>,
  ret_ty: ErlType,
  when_expr: Option<Vec<ErlType>>,
) -> IroncladResult<FnClauseType> {
  // TODO: Check name equals function name, for module level functions
  if let Some(when_expr_val) = when_expr {
    let mut substituted_args = Vec::default();
    for arg in args.into_iter() {
      // use for loop to allow the `?` operator to work
      substituted_args.push(TypeImpl::substitute_var(arg, &when_expr_val)?);
    }
    let substituted_ret_type = TypeImpl::substitute_var(ret_ty, &when_expr_val)?;
    Ok(FnClauseType::new(substituted_args, substituted_ret_type))
  } else {
    Ok(FnClauseType::new(args, ret_ty))
  }
}

/// Parses a function clause args specs, return spec and optional `when`
fn parse_fn_spec_fnclause(input: ParserInput) -> ParserResult<FnClauseType> {
  map(
    tuple((
      // Function clause name
      // opt(tok_atom),
      // Args list (list of type variables with some types possibly)
      context(
        "arguments list in a function clause spec",
        cut(parse_t_util::parse_parenthesized_arg_spec_list),
      ),
      preceded(
        tok_right_arrow,
        // Return type for fn clause
        context(
          "return type in function clause spec",
          // alt((parse_typevar_or_type, context("return type", cut(parse_type::parse_type)))),
          context("return type", cut(parse_type::parse_type)),
        ),
      ),
      // Optional: when <comma separated list of typevariables given types>
      context("when expression for typespec", opt(parse_t_util::parse_when_expr_for_type)),
    )),
    |(args, ret_ty, when_expr)| parse_fnclause_map_fn(args, ret_ty, when_expr).unwrap(),
  )(input)
}
