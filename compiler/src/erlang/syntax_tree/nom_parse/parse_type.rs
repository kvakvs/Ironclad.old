//! Contains parsers for function typespecs and type syntax.

use std::sync::Arc;
use nom::{combinator, sequence, multi, character, bytes::complete::{tag}, branch,
          combinator::{cut},
          error::{context}};

use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::nom_parse::{ErlParser, ErlParserError};
use crate::erlang::syntax_tree::nom_parse::parse_atom::AtomParser;
use crate::mfarity::MFArity;
use crate::source_loc::SourceLoc;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_clause_type::FnClauseType;
use crate::typing::typevar::Typevar;

impl ErlParser {
  /// Parse part of typevar: `:: type()`, this is to be wrapped in `branch::opt()` by the caller
  fn parse_coloncolon_type(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    let (input, _tag) = Self::ws_before(tag("::"))(input)?;
    Self::ws_before(Self::parse_type)(input)
  }

  /// Parse a capitalized type variable name with an optional `:: type()` part
  fn parse_typevar_with_opt_type(input: &str) -> nom::IResult<&str, Typevar, ErlParserError> {
    combinator::map(
      sequence::pair(
        Self::parse_typevar,
        combinator::opt(Self::parse_coloncolon_type),
      ),
      |(tv_name, maybe_type)| Typevar::new(Some(tv_name), maybe_type),
    )(input)
  }

  // /// Parses a list of comma separated typevars (function arg specs)
  // fn parse_comma_sep_arg_specs(input: &str) -> nom::IResult<&str, Vec<Typevar>> {
  //   multi::separated_list0(
  //     Self::ws(character::complete::char(',')),
  //
  //     // Comma separated arguments spec can be typevars with optional `::type()`s or just types
  //     branch::alt((
  //       Self::parse_typevar_with_opt_type,
  //       combinator::map(Self::parse_type, |t| Typevar::from_erltype(&t)),
  //     )),
  //   )(input)
  // }

  /// Parses a list of comma separated typevars enclosed in (parentheses)
  pub fn parse_parenthesized_arg_spec_list(input: &str) -> nom::IResult<&str, Vec<Typevar>, ErlParserError> {
    let (input, _) = Self::ws_before(character::complete::char('('))(input)?;
    combinator::map(
      sequence::pair(
        // Self::parse_comma_sep_arg_specs,
        Self::parse_comma_sep_typeargs0,
        Self::ws_before(character::complete::char(')')),
      ),
      |(elements, _)| elements,
    )(input)
  }

  /// Parse a `when` clause where unspecced typevars can be given types, like:
  /// `-spec fun(A) -> A when A :: atom().`
  pub fn parse_when_expr_for_type(input: &str) -> nom::IResult<&str, Vec<Typevar>, ErlParserError> {
    let (input, _) = Self::ws_before(tag("when"))(input)?;
    combinator::map(
      sequence::pair(
        Self::parse_comma_sep_typeargs1,
        Self::ws_before(character::complete::char('.')),
      ),
      |(typevars, _)| typevars,
    )(input)
  }

  /// Parses a function clause args specs, return spec and optional `when`
  fn parse_fnclause_spec(input: &str) -> nom::IResult<&str, FnClauseType, ErlParserError> {
    combinator::map(
      sequence::tuple((
        // Function clause name
        Self::ws_before_mut(combinator::opt(AtomParser::parse_atom)),

        // Args list (list of type variables with some types possibly)
        Self::parse_parenthesized_arg_spec_list,
        Self::ws_before(tag("->")),

        // Return type for fn clause
        Self::parse_typevar_with_opt_type,

        // Optional: when <comma separated list of typevariables given types>
        context("fgsfds", combinator::opt(Self::parse_when_expr_for_type)),
      )),
      |(_name, args, _arrow, ret_ty, when_expr)| {
        // TODO: Check name equals function name, for module level functions
        let merged_args = Typevar::merge_lists(&args, &when_expr.unwrap_or_else(|| vec![]));
        FnClauseType::new(merged_args, ret_ty)
      },
    )(input)
  }

  /// Given function spec module attribute `-spec name(args...) -> ...` parse into an AST node
  pub fn parse_fun_spec(input: &str) -> nom::IResult<&str, Arc<ErlAst>, ErlParserError> {
    combinator::map(
      sequence::tuple((
        Self::ws_before(character::complete::char('-')),
        Self::ws_before(tag("spec")),
        Self::ws_before(AtomParser::parse_atom),
        multi::separated_list1(
          Self::ws_before(character::complete::char(';')),
          context("parsing function clause in a function spec",
                  cut(Self::ws_before(Self::parse_fnclause_spec))),
        ),
        Self::attr_terminator,
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
  fn parse_typevar(input: &str) -> nom::IResult<&str, String, ErlParserError> {
    Self::ws_before(Self::parse_ident_capitalized)(input)
  }

  fn parse_typevar_or_type(input: &str) -> nom::IResult<&str, Typevar, ErlParserError> {
    branch::alt((
      combinator::map(Self::parse_type, |t| Typevar::from_erltype(&t)),
      combinator::map(Self::parse_typevar, |tvname| Typevar::new(Some(tvname), None)),
    ))(input)
  }

  /// Parses a comma separated list of 0 or more type arguments.
  /// A parametrized type accepts other types or typevar names
  fn parse_comma_sep_typeargs0(input: &str) -> nom::IResult<&str, Vec<Typevar>, ErlParserError> {
    multi::separated_list0(
      Self::ws(character::complete::char(',')),
      Self::parse_typevar_or_type,
    )(input)
  }

  /// Parses a comma separated list of 1 or more type arguments.
  /// A parametrized type accepts other types or typevar names
  fn parse_comma_sep_typeargs1(input: &str) -> nom::IResult<&str, Vec<Typevar>, ErlParserError> {
    multi::separated_list1(
      Self::ws(character::complete::char(',')),
      Self::parse_typevar_or_type,
    )(input)
  }

  /// Parse a user defined type with `name()` and 0 or more typevar args.
  fn parse_user_defined_type(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    combinator::map(
      sequence::tuple((
        Self::ws_before(Self::parse_ident),
        Self::ws_before(character::complete::char('(')),
        Self::parse_comma_sep_typeargs0,
        Self::ws_before(character::complete::char(')')),
      )),
      |(type_name, _open, elements, _close)| {
        ErlType::from_name(type_name, &elements).into()
      },
    )(input)
  }

  /// Parse a list of types, returns a temporary list-type
  fn parse_type_list(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    let (input, _open_tag) = Self::ws_before(character::complete::char('['))(input)?;

    combinator::map(
      sequence::terminated(
        Self::parse_comma_sep_typeargs0,
        Self::ws_before(character::complete::char(']')),
      ),
      |elements| ErlType::TypevarList(elements).into(),
    )(input)
  }

  /// Parse a tuple of types, returns a temporary tuple-type
  fn parse_type_tuple(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    let (input, _open_tag) = Self::ws_before(character::complete::char('{'))(input)?;

    combinator::map(
      sequence::terminated(
        Self::parse_comma_sep_typeargs0,
        Self::ws_before(character::complete::char('}')),
      ),
      |elements| ErlType::TypevarList(elements).into(),
    )(input)
  }

  /// Parse any Erlang type, simple types like `atom()` with some `(args)` possibly, but could also be
  /// a structured type like union of multiple types `atom()|number()`, a list or a tuple of types, etc
  pub fn parse_type(input: &str) -> nom::IResult<&str, Arc<ErlType>, ErlParserError> {
    branch::alt((
      Self::parse_type_list,
      Self::parse_type_tuple,
      Self::parse_user_defined_type,
    ))(input)
  }
}