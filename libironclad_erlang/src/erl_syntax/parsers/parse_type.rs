//! Contains parsers for function typespecs and type syntax.

use crate::erl_syntax::erl_ast::{ErlAst, ErlAstType};
use crate::erl_syntax::parsers::misc::{
  colon_colon, comma, curly_close, curly_open, dot_dot, hash_symbol, match_dash_tag, par_close,
  par_open, parse_int, parse_varname, period_newline, semicolon, square_close, square_open,
  ws_before,
};
use crate::erl_syntax::parsers::parse_atom::AtomParser;
use crate::erl_syntax::parsers::{AstParserResult, ErlParserError};
use crate::literal::Literal;
use crate::typing::erl_integer::ErlInteger;
use crate::typing::erl_type::map_type::MapMemberType;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_clause_type::FnClauseType;
use crate::typing::typevar::Typevar;
use libironclad_error::source_loc::SourceLoc;
use libironclad_util::mfarity::MFArity;
use nom::branch::alt;
use nom::combinator::{cut, map, opt};
use nom::multi::{separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple};
use nom::{bytes::complete::tag, character::complete::char, error::context};
use std::sync::Arc;

/// Holds code for parsing types and typespecs
pub struct ErlTypeParser {}

impl ErlTypeParser {
  /// Given function spec module attribute `-spec name(args...) -> ...` parse into an AST node
  /// Dash `-` is matched outside by the caller.
  pub fn fn_spec_attr(input: &str) -> AstParserResult {
    map(
      // all between -spec and .
      delimited(
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
        period_newline,
      ),
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

    terminated(Self::comma_sep_typeargs0, par_close)(input)
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
  fn comma_sep_typeargs0(input: &str) -> nom::IResult<&str, Vec<Typevar>, ErlParserError> {
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
  fn user_defined_type(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    map(
      tuple((
        opt(Self::parse_type_modulename_colon),
        AtomParser::atom,
        delimited(
          par_open,
          context("type arguments for a user-defined type", Self::comma_sep_typeargs0),
          par_close,
        ),
      )),
      |(maybe_module, type_name, elements)| ErlType::from_name(maybe_module, type_name, &elements),
    )(input)
  }

  /// Parse a record type reference with `#tagname{}`, does not define a record, refers to an existing
  fn record_ref(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    map(
      preceded(hash_symbol, pair(AtomParser::atom, pair(curly_open, curly_close))),
      |(tag, (_, _))| ErlType::new_record_ref(tag),
    )(input)
  }

  /// Parse a list of types, returns a temporary list-type
  fn type_of_list(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    map(
      delimited(
        square_open,
        context("type arguments for a list() type", Self::comma_sep_typeargs0),
        square_close,
      ),
      |elements| {
        let typevar_types = Typevar::vec_of_typevars_into_types(elements);
        ErlType::list_of_types(typevar_types)
      },
    )(input)
  }

  /// Parse a tuple of types, returns a temporary tuple-type
  fn type_of_tuple(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    map(
      delimited(curly_open, context("a tuple() type", Self::comma_sep_typeargs0), curly_close),
      |elements| {
        let typevar_types = Typevar::vec_of_typevars_into_types(elements);
        ErlType::new_tuple_move(typevar_types)
      },
    )(input)
  }

  fn map_member_type(input: &str) -> nom::IResult<&str, MapMemberType, ErlParserError> {
    map(
      separated_pair(Self::alt_typevar_or_type, ws_before(tag("=>")), Self::alt_typevar_or_type),
      |(key, value)| MapMemberType {
        key: ErlType::new_typevar(key),
        value: ErlType::new_typevar(value),
      },
    )(input)
  }

  /// Parses a comma separated list of map field types
  fn comma_sep_map_members0(input: &str) -> nom::IResult<&str, Vec<MapMemberType>, ErlParserError> {
    separated_list0(comma, context("parsing member types of a map type", Self::map_member_type))(
      input,
    )
  }

  /// Parse a map of types, returns a map-type
  fn type_of_map(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    map(
      delimited(
        pair(hash_symbol, curly_open),
        context("a map() type", Self::comma_sep_map_members0),
        curly_close,
      ),
      ErlType::new_map,
    )(input)
  }

  /// Parse an integer and produce a literal integer type
  pub fn int_literal_type(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    map(parse_int, |i_str| {
      // TODO: Can a parsed integer parse with an error?
      let i = ErlInteger::new_from_string(i_str).unwrap();
      ErlType::new_singleton(&Literal::Integer(i).into())
    })(input)
  }

  /// Parse an integer range
  pub fn int_range_type(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    // print_input("int_range_type", input);
    map(separated_pair(parse_int, dot_dot, parse_int), |(a_str, b_str)| {
      // TODO: Can a parsed integer parse with an error?
      let a = ErlInteger::new_from_string(a_str).unwrap();
      let b = ErlInteger::new_from_string(b_str).unwrap();
      ErlType::new_range(a, b)
    })(input)
  }

  /// Parse an atom, and produce a literal atom type
  pub fn atom_literal_type(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    map(AtomParser::atom, |a_str| ErlType::new_singleton(&Literal::Atom(a_str).into()))(input)
  }

  /// Parse any simple Erlang type without union. To parse unions use `parse_type`.
  pub fn parse_nonunion_type(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    alt((
      Self::int_range_type,
      Self::type_of_list,
      Self::type_of_tuple,
      Self::type_of_map,
      Self::record_ref,
      Self::user_defined_type,
      Self::int_literal_type,
      Self::atom_literal_type,
    ))(input)
  }

  /// Parse any Erlang type, simple types like `atom()` with some `(args)` possibly, but could also be
  /// a structured type like union of multiple types `atom()|number()`, a list or a tuple of types, etc
  pub fn parse_type(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    map(
      separated_list1(ws_before(char('|')), ws_before(Self::parse_nonunion_type)),
      |types| ErlType::new_union(&types),
    )(input)
  }

  /// Wraps parsed type into a type-AST-node
  pub fn parse_type_node(input: &str) -> AstParserResult {
    map(Self::parse_type, |t| {
      ErlAst::construct_with_location(&SourceLoc::from_input(input), ErlAstType::Type { ty: t })
    })(input)
  }
}
