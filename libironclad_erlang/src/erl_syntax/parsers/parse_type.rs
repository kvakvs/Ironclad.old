//! Contains parsers for function typespecs and type syntax.

use crate::erl_syntax::erl_ast::{ErlAst, ErlAstType};
use crate::erl_syntax::parsers::misc::{
  colon_colon, comma, match_dash_tag, par_close, par_open, parse_int, parse_varname, semicolon,
  ws_before,
};
use crate::erl_syntax::parsers::parse_atom::AtomParser;
use crate::erl_syntax::parsers::{AstParserResult, ErlParserError};
use crate::literal::Literal;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_clause_type::FnClauseType;
use crate::typing::typevar::Typevar;
use libironclad_error::source_loc::SourceLoc;
use libironclad_util::mfarity::MFArity;
use nom::branch::alt;
use nom::combinator::{cut, map, opt};
use nom::multi::{separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::{bytes::complete::tag, character::complete::char, error::context};
use std::sync::Arc;

/// Holds code for parsing types and typespecs
pub struct ErlTypeParser {}

impl ErlTypeParser {
  /// Given function spec module attribute `-spec name(args...) -> ...` parse into an AST node
  /// Dash `-` is matched outside by the caller.
  pub fn fn_spec_attr(input: &str) -> AstParserResult {
    map(
      preceded(
        match_dash_tag("spec"),
        tuple((
          context("Function name in a -spec() attribute", cut(AtomParser::atom)),
          separated_list1(
            semicolon,
            context(
              "Function clause in a -spec() attribute",
              cut(ws_before(Self::parse_fn_spec_fnclause)),
            ),
          ),
        )),
      ), // preceded by -spec
      |(name, clauses)| {
        let arity = clauses[0].arity();
        assert!(
          clauses.iter().all(|c| c.arity() == arity),
          "All function clauses must have same arity in a typespec"
        );
        let funarity = MFArity::new_local(&name, arity);
        let fntypespec = ErlType::new_fn_type(&clauses);
        ErlAst::new_fn_spec(&SourceLoc::from_input(input), funarity, fntypespec.into())
      },
    )(input)
  }

  /// Parses a function clause args specs, return spec and optional `when`
  fn parse_fn_spec_fnclause(input: &str) -> nom::IResult<&str, FnClauseType, ErlParserError> {
    map(
      tuple((
        // Function clause name
        opt(AtomParser::atom),
        // Args list (list of type variables with some types possibly)
        context(
          "arguments list in a function clause spec",
          Self::parse_parenthesized_arg_spec_list,
        ),
        ws_before(tag("->")),
        // Return type for fn clause
        context(
          "return type in function clause spec",
          alt((Self::parse_typevar_with_opt_type, Self::parse_type_as_typevar)),
        ),
        // Optional: when <comma separated list of typevariables given types>
        context("when expression for typespec", opt(Self::parse_when_expr_for_type)),
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

  /// Parse part of typevar: `:: type()`, this is to be wrapped in `branch::opt()` by the caller
  fn parse_coloncolon_type(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    preceded(
      colon_colon,
      context("Type ascription or a type after ::", ws_before(Self::parse_type)),
    )(input)
  }

  /// Parse a capitalized type variable name with an optional `:: type()` part:
  /// `A :: type()` or `A`
  fn parse_typevar_with_opt_type(input: &str) -> nom::IResult<&str, Typevar, ErlParserError> {
    map(
      pair(Self::parse_typevar_name, opt(Self::parse_coloncolon_type)),
      |(tv_name, maybe_type)| Typevar::new(Some(tv_name), maybe_type),
    )(input)
  }

  fn parse_type_as_typevar(input: &str) -> nom::IResult<&str, Typevar, ErlParserError> {
    map(Self::parse_type, |t| Typevar::from_erltype(&t))(input)
  }

  // /// Parses a list of comma separated typevars (function arg specs)
  // fn parse_comma_sep_arg_specs(input: &str) -> nom::IResult<&str, Vec<Typevar>> {
  //   separated_list0(
  //     Self::ws(char(',')),
  //
  //     // Comma separated arguments spec can be typevars with optional `::type()`s or just types
  //     alt((
  //       Self::parse_typevar_with_opt_type,
  //       map(Self::parse_type, |t| Typevar::from_erltype(&t)),
  //     )),
  //   )(input)
  // }

  /// Parses a list of comma separated typevars enclosed in (parentheses)
  pub fn parse_parenthesized_arg_spec_list(
    input: &str,
  ) -> nom::IResult<&str, Vec<Typevar>, ErlParserError> {
    let (input, _) = par_open(input)?;

    terminated(Self::parse_comma_sep_typeargs0, par_close)(input)
  }

  /// Parse a `when` clause where unspecced typevars can be given types, like:
  /// `-spec fun(A) -> A when A :: atom().`
  pub fn parse_when_expr_for_type(input: &str) -> nom::IResult<&str, Vec<Typevar>, ErlParserError> {
    let (input, _) = ws_before(tag("when"))(input)?;
    Self::parse_comma_sep_typeargs1(input)
  }

  /// Parse only capitalized type variable name
  pub fn parse_typevar_name(input: &str) -> nom::IResult<&str, String, ErlParserError> {
    ws_before(parse_varname)(input)
  }

  fn alt_typevar_or_type(input: &str) -> nom::IResult<&str, Typevar, ErlParserError> {
    alt((
      Self::parse_typevar_with_opt_type,
      map(Self::parse_type, |t| Typevar::from_erltype(&t)),
      // map(Self::parse_typevar, |tvname| Typevar::new(Some(tvname), None)),
    ))(input)
  }

  #[allow(dead_code)]
  fn parse_typearg(input: &str) -> nom::IResult<&str, Typevar, ErlParserError> {
    map(ws_before(Self::parse_type), |t| Typevar::from_erltype(&t))(input)
  }

  /// Parses a comma separated list of 0 or more type arguments.
  /// A parametrized type accepts other types or typevar names
  fn parse_comma_sep_typeargs0(input: &str) -> nom::IResult<&str, Vec<Typevar>, ErlParserError> {
    separated_list0(comma, context("parsing items of a typeargs0_list", Self::alt_typevar_or_type))(
      input,
    )
  }

  /// Parses a comma separated list of 1 or more type arguments.
  /// A parametrized type accepts other types or typevar names
  fn parse_comma_sep_typeargs1(input: &str) -> nom::IResult<&str, Vec<Typevar>, ErlParserError> {
    separated_list1(comma, context("parsing items of a typeargs1_list", Self::alt_typevar_or_type))(
      input,
    )
  }

  /// Optional `module:` before typename in `module:type()`.
  fn parse_type_modulename_colon(input: &str) -> nom::IResult<&str, String, ErlParserError> {
    terminated(AtomParser::atom, ws_before(char(':')))(input)
  }

  /// Parse a user defined type with `name()` and 0 or more typevar args.
  /// Optional with module name `module:name()`.
  fn parse_user_defined_type(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    map(
      tuple((
        opt(Self::parse_type_modulename_colon),
        AtomParser::atom,
        delimited(
          par_open,
          context("type arguments for a user-defined type", Self::parse_comma_sep_typeargs0),
          par_close,
        ),
      )),
      |(maybe_module, type_name, elements)| ErlType::from_name(maybe_module, type_name, &elements),
    )(input)
  }

  /// Parse a list of types, returns a temporary list-type
  fn parse_type_list(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    let (input, _open_tag) = ws_before(char('['))(input)?;

    map(
      terminated(
        context("type arguments for a list() type", Self::parse_comma_sep_typeargs0),
        ws_before(char(']')),
      ),
      |elements| {
        let typevar_types = Typevar::vec_of_typevars_into_types(elements);
        ErlType::list_of_types(typevar_types)
      },
    )(input)
  }

  /// Parse a tuple of types, returns a temporary tuple-type
  fn parse_type_tuple(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    let (input, _open_tag) = ws_before(char('{'))(input)?;

    map(
      terminated(
        context("type arguments for a tuple() type", Self::parse_comma_sep_typeargs0),
        ws_before(char('}')),
      ),
      |elements| {
        let typevar_types = Typevar::vec_of_typevars_into_types(elements);
        ErlType::new_tuple_move(typevar_types)
      },
    )(input)
  }

  /// Parse an integer and produce a literal integer type
  pub fn parse_int_lit_type(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    map(parse_int, |i_str| {
      let i = i_str.parse().unwrap(); // TODO: Support big integers
      ErlType::new_singleton(&Literal::Integer(i).into())
    })(input)
  }

  /// Parse an atom, and produce a literal atom type
  pub fn parse_atom_lit_type(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    map(AtomParser::atom, |a_str| ErlType::new_singleton(&Literal::Atom(a_str).into()))(input)
  }

  /// Parse any simple Erlang type without union. To parse unions use `parse_type`.
  pub fn parse_nonunion_type(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    alt((
      Self::parse_type_list,
      Self::parse_type_tuple,
      Self::parse_user_defined_type,
      Self::parse_int_lit_type,
      Self::parse_atom_lit_type,
    ))(input)
  }

  /// Parse any Erlang type, simple types like `atom()` with some `(args)` possibly, but could also be
  /// a structured type like union of multiple types `atom()|number()`, a list or a tuple of types, etc
  pub fn parse_type(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    map(
      separated_list1(ws_before(char('|')), ws_before(Self::parse_nonunion_type)),
      |types| ErlType::new_union_skip_normalize(&types),
    )(input)
  }

  /// Wraps parsed type into a type-AST-node
  pub fn parse_type_node(input: &str) -> AstParserResult {
    map(Self::parse_type, |t| {
      ErlAst::construct_with_location(&SourceLoc::from_input(input), ErlAstType::Type { ty: t })
    })(input)
  }
}
