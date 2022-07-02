//! Parse records in expressions

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_record::RecordBuilderMember;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{
  tok, tok_atom, tok_comma, tok_curly_close, tok_curly_open, tok_hash,
};
use crate::erl_syntax::parsers::parse_expr;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::source_loc::SourceLoc;
use nom::combinator::map;
use nom::multi::separated_list0;
use nom::sequence::{delimited, pair, separated_pair, terminated};

/// Parse one member of a record builder `'field' = EXPR`
fn record_builder_member(input: ParserInput) -> ParserResult<RecordBuilderMember> {
  map(
    separated_pair(tok_atom, tok(TokenType::EqualSymbol), parse_expr::parse_expr),
    |(field, expr)| RecordBuilderMember { field, expr },
  )(input)
}

/// Parse a record builder expression
pub fn parse_record_builder(
  input: ParserInput,
) -> ParserResult<(String, Vec<RecordBuilderMember>)> {
  terminated(
    pair(
      delimited(tok_hash, tok_atom, tok_curly_open),
      separated_list0(tok_comma, record_builder_member),
    ),
    tok_curly_close,
  )(input.clone())
}

/// Parse a record builder expression without a prefix expression just `# RECORDTAG { FIELDS }`
pub fn parse_record_builder_no_base(input: ParserInput) -> ParserResult<AstNode> {
  map(parse_record_builder, |(tag, record_fields)| {
    AstNodeImpl::new_record_builder(SourceLoc::new(&input), None, tag, record_fields)
  })(input.clone())
}
