//! Record syntax parser support

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_record::RecordField;
use crate::erl_syntax::parsers::defs::{ParserInput, ParserResult};
use crate::erl_syntax::parsers::misc::{
  colon_colon_tag, comma_tag, curly_close_tag, curly_open_tag, equals_tag, match_dash_tag,
  par_close_tag, par_open_tag, period_newline_tag,
};
use crate::erl_syntax::parsers::parse_atom::parse_atom;
use crate::erl_syntax::parsers::parse_type::ErlTypeParser;
use crate::erl_syntax::parsers::ErlParser;
use nom::combinator::{cut, map, opt};
use nom::error::context;
use nom::multi::separated_list0;
use nom::sequence::{delimited, preceded, separated_pair, tuple};

/// Parses one field from the field list of `-record(atom(), { <FIELDS> } ).`.
/// The field parser has a structure: `ATOM ( = EXPR ) ( :: TYPE )`
fn record_definition_one_field(input: ParserInput) -> ParserResult<RecordField> {
  map(
    tuple((
      parse_atom,
      opt(preceded(equals_tag, ErlParser::parse_expr)),
      opt(preceded(colon_colon_tag, ErlTypeParser::parse_type)),
    )),
    |(field_tag, opt_initializer, opt_type)| RecordField {
      field_tag,
      initializer: opt_initializer,
      type_ascription: opt_type,
    },
  )(input)
}

/// Parses inner fields list of `-record(atom, { <FIELDS> } ).`
fn record_definition_fields(input: ParserInput) -> ParserResult<Vec<RecordField>> {
  delimited(
    curly_open_tag,
    separated_list0(
      comma_tag,
      context("record definition field", cut(record_definition_one_field)),
    ),
    curly_close_tag,
  )(input)
}

/// Parses inner contents of `-record( <INNER> ).`
fn record_definition_inner(input: ParserInput) -> ParserResult<AstNode> {
  map(
    separated_pair(parse_atom, comma_tag, record_definition_fields),
    |(atom, fields)| AstNodeImpl::new_record_definition(input.loc(), atom, fields),
  )(input.clone())
}

/// Parses a `-record(atom(), {field :: type()... }).` attribute.
pub fn parse_record_def(input: ParserInput) -> ParserResult<AstNode> {
  delimited(
    match_dash_tag("record".into()),
    context(
      "record definition in a -record() attribute",
      cut(delimited(par_open_tag, record_definition_inner, par_close_tag)),
    ),
    period_newline_tag,
  )(input)
}
