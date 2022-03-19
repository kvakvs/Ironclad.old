//! Parse binary expressions and binary builders.

use std::ops::Deref;
use std::str::FromStr;
use crate::erlang::syntax_tree::nom_parse::{AstParserResult, ErlParser, ParserResult};
use nom::{branch, bytes::complete::{tag}, character::complete::{char}, error::{context},
          combinator, combinator::{cut}, sequence, multi};
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::node::erl_binary_element::{BinaryElement, TypeSpecifier, ValueEndianness, ValueSignedness, ValueType, ValueWidth};
use crate::literal::Literal;
use crate::source_loc::SourceLoc;

/// Wraps functions for parsing binaries and binary builders
pub struct BinaryParser {}

impl BinaryParser {
  /// Parse a literal value, variable, or an expression in parentheses.
  fn parse_value(input: &str) -> AstParserResult {
    branch::alt((
      combinator::map(
        ErlParser::parse_varname,
        |v| ErlAst::new_var(SourceLoc::None, &v)),
      ErlParser::parse_literal,
      sequence::delimited(
        ErlParser::ws_before(char('(')),
        ErlParser::parse_expr,
        ErlParser::ws_before(char(')')),
      ),
    ))(input)
  }

  /// Parse a `:Number`, `:Variable` or `:(Expr)` for bit width
  fn parse_width(input: &str) -> ParserResult<ValueWidth> {
    combinator::map(
      Self::parse_value,
      |v| {
        if let ErlAst::Lit { value: lit_val, .. } = v.deref() {
          // TODO: Big Integer
          if let Literal::Integer(inner_val) = lit_val.deref() {
            assert!(*inner_val > 0);
            ValueWidth::Literal(inner_val.abs() as usize)
          } else {
            panic!("For bit width only positive integers are accepted")
          }
        } else {
          ValueWidth::Expr(v)
        }
      },
    )(input)
  }

  fn parse_typespec_type(input: &str) -> ParserResult<TypeSpecifier> {
    branch::alt((
      combinator::map(tag("integer"), |_| TypeSpecifier::Type(ValueType::Integer)),
      combinator::map(tag("float"), |_| TypeSpecifier::Type(ValueType::Float)),
      combinator::map(
        branch::alt((tag("bytes"), tag("binary"))),
        |_| TypeSpecifier::Type(ValueType::Bytes)),
      combinator::map(
        branch::alt((tag("bitstring"), tag("bits"))),
        |_| TypeSpecifier::Type(ValueType::Bitstring)),
      combinator::map(tag("utf8"), |_| TypeSpecifier::Type(ValueType::Utf8)),
      combinator::map(tag("utf16"), |_| TypeSpecifier::Type(ValueType::Utf16)),
      combinator::map(tag("utf32"), |_| TypeSpecifier::Type(ValueType::Utf32)),
    ))(input)
  }

  fn parse_typespec_signedness(input: &str) -> ParserResult<TypeSpecifier> {
    branch::alt((
      combinator::map(tag("signed"), |_| TypeSpecifier::Signedness(ValueSignedness::Signed)),
      combinator::map(tag("unsigned"), |_| TypeSpecifier::Signedness(ValueSignedness::Unsigned)),
    ))(input)
  }

  fn parse_typespec_endianness(input: &str) -> ParserResult<TypeSpecifier> {
    branch::alt((
      combinator::map(tag("big"), |_| TypeSpecifier::Endianness(ValueEndianness::Big)),
      combinator::map(tag("little"), |_| TypeSpecifier::Endianness(ValueEndianness::Little)),
      combinator::map(tag("native"), |_| TypeSpecifier::Endianness(ValueEndianness::Native)),
    ))(input)
  }

  fn parse_typespec_unit(input: &str) -> ParserResult<TypeSpecifier> {
    combinator::map(
      sequence::preceded(
        tag("unit:"),
        ErlParser::parse_int),
      |i_str| TypeSpecifier::Unit(usize::from_str(i_str).unwrap()),
    )(input)
  }

  fn parse_a_type_spec(input: &str) -> ParserResult<TypeSpecifier> {
    branch::alt((
      Self::parse_typespec_type,
      Self::parse_typespec_signedness,
      Self::parse_typespec_endianness,
      Self::parse_typespec_unit,
    ))(input)
  }

  /// Parse a `-` separated list of typespecs
  fn parse_type_specs(input: &str) -> ParserResult<Vec<TypeSpecifier>> {
    sequence::preceded(
      ErlParser::ws_before(char('/')), // TODO: Whitespace allowed before?
      multi::separated_list1(
        ErlParser::ws_before(char('-')), // TODO: Whitespace allowed before?
        Self::parse_a_type_spec,
      ),
    )(input)
  }

  /// Parse one comma-separated element of a binary: number, variable, an expression,
  /// and followed by a `:bit-width` and `/type-specifier`
  fn parse_bin_element(input: &str) -> ParserResult<BinaryElement> {
    combinator::map(
      sequence::tuple((
        Self::parse_value,
        combinator::opt(
          sequence::preceded(
            ErlParser::ws_before(char(':')),
            Self::parse_width)),
        combinator::opt(
          Self::parse_type_specs,
        ),
      )),
      |(value, bit_width, type_specs)| {
        BinaryElement::new(SourceLoc::None,
                           value,
                           bit_width.unwrap_or(ValueWidth::Default),
                           type_specs.unwrap_or_default())
      },
    )(input)
  }

  /// Parse a binary or binary builder expression
  pub fn parse(input: &str) -> AstParserResult {
    let (input, _) = ErlParser::ws_before(tag("<<"))(input)?;

    context("binary expression", cut(
      sequence::terminated(
        combinator::map(
          multi::separated_list1(
            ErlParser::ws_before(char(',')),
            context("binary expression element", cut(
              ErlParser::ws_before(Self::parse_bin_element)
            )),
          ),
          |bin_exprs| {
            ErlAst::new_binary_expr(SourceLoc::None, bin_exprs)
          },
        ),
        ErlParser::ws_before(tag(">>")),
      )
    ))(input)
  }
}
