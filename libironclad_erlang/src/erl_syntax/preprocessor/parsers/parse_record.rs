//! Record syntax parser support

use crate::erl_syntax::node::erl_record::RecordField;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{
  dash_atom, period_eol, tok, tok_atom, tok_comma, tok_curly_close, tok_curly_open, tok_par_close,
  tok_par_open,
};
use crate::erl_syntax::parsers::parse_expr::parse_expr;
use crate::erl_syntax::parsers::parse_type::ErlTypeParser;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::preprocessor::pp_node::pp_impl::PreprocessorNodeImpl;
use crate::erl_syntax::preprocessor::pp_node::PreprocessorNode;
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
    tok_curly_open,
    separated_list0(
      tok_comma,
      context("record definition field", cut(record_definition_one_field)),
    ),
    tok_curly_close,
  )(input)
}

/// Parses inner contents of `-record( <INNER> ).`
fn record_definition_inner(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(
    separated_pair(tok_atom, tok_comma, record_definition_fields),
    |(atom, fields)| {
      PreprocessorNodeImpl::new_record_definition(SourceLoc::new(&input), atom, fields)
    },
  )(input.clone())
}

/// Parses a `-record(atom(), {field :: type()... }).` attribute.
pub fn parse_record_def(input: ParserInput) -> ParserResult<PreprocessorNode> {
  delimited(
    |i1| dash_atom(i1, "record"),
    context(
      "record definition in a -record() attribute",
      cut(delimited(tok_par_open, record_definition_inner, tok_par_close)),
    ),
    period_eol,
  )(input)
}
