//! Parse binary expressions and binary builders.

use std::ops::Deref;
use crate::erlang::syntax_tree::nom_parse::{AstParserResult, ErlParser, ParserResult};
use nom::{branch, bytes::complete::{tag}, character::complete::{char}, error::{context},
          combinator, combinator::{cut}, sequence, multi};
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::node::erl_binary_element::{BinaryElement, TypeSpecifier, ValueWidth};
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

  /// Parse a - separated list of typespecs
  fn parse_type_specs(input: &str) -> ParserResult<Vec<TypeSpecifier>> {
    Ok((input, Vec::default()))
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
        Self::parse_type_specs,
      )),
      |(value, bit_width, type_specs)| {
        BinaryElement::new(SourceLoc::None,
                           value,
                           bit_width.unwrap_or(ValueWidth::Default),
                           type_specs)
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
