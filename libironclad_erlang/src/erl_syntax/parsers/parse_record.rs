//! Record syntax parser support

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_record::RecordField;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{period_newline, tok, tok_atom, tok_atom_of};
use crate::erl_syntax::parsers::parse_expr::parse_expr;
use crate::erl_syntax::parsers::parse_type::ErlTypeParser;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::source_loc::SourceLoc;
use nom::combinator::{cut, map, opt};
use nom::error::context;
use nom::multi::separated_list0;
use nom::sequence::{delimited, preceded, separated_pair, tuple};

/// Parses one field from the field list of `-record(atom(), { <FIELDS> } ).`.
/// The field parser has a structure: `ATOM ( = EXPR ) ( :: TYPE )`
fn record_definition_one_field(input: ParserInput) -> ParserResult<RecordField> {
  map(
    tuple((
      tok_atom,
      opt(preceded(
        tok(TokenType::EqualSymbol),
        context("default value for a field", cut(parse_expr)),
      )),
      opt(preceded(
        tok(TokenType::ColonColon),
        context("type ascription for a field", cut(ErlTypeParser::parse_type)),
      )),
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
    tok(TokenType::CurlyOpen),
    separated_list0(
      tok(TokenType::Comma),
      context("record definition field", cut(record_definition_one_field)),
    ),
    tok(TokenType::CurlyClose),
  )(input)
}

/// Parses inner contents of `-record( <INNER> ).`
fn record_definition_inner(input: ParserInput) -> ParserResult<AstNode> {
  map(
    separated_pair(tok_atom, tok(TokenType::Comma), record_definition_fields),
    |(atom, fields)| AstNodeImpl::new_record_definition(SourceLoc::new(&input), atom, fields),
  )(input.clone())
}

/// Parses a `-record(atom(), {field :: type()... }).` attribute.
pub fn parse_record_def(input: ParserInput) -> ParserResult<AstNode> {
  delimited(
    tok_atom_of("record"),
    context(
      "record definition in a -record() attribute",
      cut(delimited(
        tok(TokenType::ParOpen),
        record_definition_inner,
        tok(TokenType::ParClose),
      )),
    ),
    period_newline,
  )(input)
}
