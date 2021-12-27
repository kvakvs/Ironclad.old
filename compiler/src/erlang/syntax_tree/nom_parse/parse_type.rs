//! Contains parsers for function typespecs and type syntax.

use std::sync::Arc;
use nom::{combinator, sequence, multi, character, bytes::complete::{tag}, branch};

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::nom_parse::{misc, parse_atom, parse_attr};
use crate::mfarity::MFArity;
use crate::source_loc::SourceLoc;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_clause_type::FnClauseType;
use crate::typing::typevar::Typevar;

/// Parse optional part of typevar: `:: type()`
fn parse_maybe_type(input: &str) -> nom::IResult<&str, Arc<ErlType>> {
  combinator::map(
    sequence::pair(
      misc::ws_before(tag("::")),
      misc::ws_before(parse_type),
    ),
    |(_, t)| t,
  )(input)
}

/// Parse a capitalized type variable name with an optional `:: type()` part
fn parse_typevar_with_opt_type(input: &str) -> nom::IResult<&str, Typevar> {
  combinator::map(
    sequence::pair(
      parse_typevar,
      combinator::opt(parse_maybe_type),
    ),
    |(tv_name, maybe_type)| Typevar::new(Some(tv_name), maybe_type),
  )(input)
}

/// Parses a list of comma separated typevars (function arg specs)
fn parse_comma_sep_arg_specs(input: &str) -> nom::IResult<&str, Vec<Typevar>> {
  multi::separated_list0(
    misc::ws(character::complete::char(',')),
    parse_typevar_with_opt_type,
  )(input)
}

/// Parses a list of comma separated typevars enclosed in (parentheses)
pub fn parse_parenthesized_arg_spec_list(input: &str) -> nom::IResult<&str, Vec<Typevar>> {
  let (input, _) = misc::ws_before(character::complete::char('('))(input)?;
  combinator::map(
    sequence::pair(
      parse_comma_sep_arg_specs,
      misc::ws_before(character::complete::char(')')),
    ),
    |(elements, _)| elements,
  )(input)
}

/// Parse a `when` clause where unspecced typevars can be given types, like:
/// `-spec fun(A) -> A when A :: atom().`
fn parse_when_expr(input: &str) -> nom::IResult<&str, Vec<Typevar>> {
  let (input, _) = misc::ws_before(tag("when"))(input)?;
  combinator::map(
    sequence::pair(
      parse_comma_sep_arg_specs,
      misc::ws_before(character::complete::char('.')),
    ),
    |(typevars, _)| typevars,
  )(input)
}

/// Parses a function clause args specs, return spec and optional `when`
fn parse_fnclause_spec(input: &str) -> nom::IResult<&str, FnClauseType> {
  combinator::map(
    sequence::tuple((
      // Function clause name
      misc::ws_before_mut(combinator::opt(parse_atom::atom)),

      // Args list
      parse_parenthesized_arg_spec_list,
      misc::ws_before(tag("->")),

      // Body
      parse_type,

      // Optional: when <guard>
      combinator::opt(parse_when_expr),
    )),
    |(name, args, _arrow, ret_ty, when_expr)| {
      let merged_args = Typevar::merge_lists(&args, &when_expr.unwrap_or_else(|| vec![]));
      FnClauseType::new(args, ret_ty)
    },
  )(input)
}

/// Given function spec module attribute `-spec name(args...) -> ...` parse into an AST node
pub fn parse_fun_spec(input: &str) -> nom::IResult<&str, Arc<ErlAst>> {
  combinator::map(
    sequence::tuple((
      misc::ws_before(character::complete::char('-')),
      misc::ws_before(tag("spec")),
      misc::ws_before(parse_atom::atom),
      multi::separated_list1(
        misc::ws_before(character::complete::char(';')),
        misc::ws_before(parse_fnclause_spec),
      ),
      parse_attr::attr_terminator,
    )),
    |(_minus, _spec, name, clauses, _term)| {
      let arity = clauses[0].arity();
      assert!(clauses.iter().all(|c| c.arity() == arity),
              "All function clauses must have same arity in a typespec");
      let funarity = MFArity::new_local(&name, arity);
      let fntypespec = ErlType::new_fn_type(&clauses);
      let fnspec = ErlAst::FnSpec {
        location: SourceLoc::None,
        funarity,
        spec: fntypespec.into(),
      };
      fnspec.into()
    },
  )(input)
}

/// Parse only capitalized type variable name
fn parse_typevar(input: &str) -> nom::IResult<&str, String> {
  misc::ws_before(misc::parse_ident_capitalized)(input)
}

/// Parses a comma separated list of type arguments.
/// A parametrized type accepts other types or typevar names
fn parse_comma_sep_typeargs(input: &str) -> nom::IResult<&str, Vec<Typevar>> {
  multi::separated_list0(
    misc::ws(character::complete::char(',')),
    branch::alt((
      combinator::map(parse_type, |t| Typevar::from_erltype(&t)),
      combinator::map(parse_typevar, |tvname| Typevar::new(Some(tvname), None)),
    )),
  )(input)
}

/// Parse a user defined type with `name()` and 0 or more typevar args.
fn parse_user_defined_type(input: &str) -> nom::IResult<&str, Arc<ErlType>> {
  combinator::map(
    sequence::tuple((
      misc::ws_before(misc::parse_ident),
      misc::ws_before(character::complete::char('(')),
      parse_comma_sep_typeargs,
      misc::ws_before(character::complete::char(')')),
    )),
    |(type_name, _open, elements, _close)| {
      ErlType::from_name(type_name, &elements).into()
    },
  )(input)
}

/// Parse a list of types, returns a temporary list-type
fn parse_type_list(input: &str) -> nom::IResult<&str, Arc<ErlType>> {
  let (input, _open_tag) = misc::ws_before(character::complete::char('['))(input)?;

  combinator::map(
    sequence::terminated(
      parse_comma_sep_typeargs,
      misc::ws_before(character::complete::char(']')),
    ),
    |elements| ErlType::TypevarList(elements).into(),
  )(input)
}

/// Parse a tuple of types, returns a temporary tuple-type
fn parse_type_tuple(input: &str) -> nom::IResult<&str, Arc<ErlType>> {
  let (input, _open_tag) = misc::ws_before(character::complete::char('{'))(input)?;

  combinator::map(
    sequence::terminated(
      parse_comma_sep_typeargs,
      misc::ws_before(character::complete::char('}')),
    ),
    |elements| ErlType::TypevarList(elements).into(),
  )(input)
}

/// Parse any Erlang type, simple types like `atom()` with some `(args)` possibly, but could also be
/// a structured type like union of multiple types `atom()|number()`, a list or a tuple of types, etc
pub fn parse_type(input: &str) -> nom::IResult<&str, Arc<ErlType>> {
  branch::alt((
    parse_type_list,
    parse_type_tuple,
    parse_user_defined_type,
  ))(input)
}
