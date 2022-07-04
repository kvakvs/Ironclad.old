//! Parse records in expressions

use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_record::RecordBuilderMember;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::tok_atom;
use crate::erl_syntax::parsers::misc_tok::*;
use crate::erl_syntax::parsers::parse_expr::parse_expr;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::source_loc::SourceLoc;
use nom::combinator::{consumed, map};
use nom::multi::separated_list0;
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple};

/// Parse one member of a record builder `'field' = EXPR`
fn record_builder_member(input: ParserInput) -> ParserResult<RecordBuilderMember> {
  map(separated_pair(tok_atom, tok_equal_symbol, parse_expr), |(field, expr)| {
    RecordBuilderMember { field, expr }
  })(input)
}

/// Parse a record builder expression
fn record_builder_tag_body(input: ParserInput) -> ParserResult<(String, Vec<RecordBuilderMember>)> {
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
  map(record_builder_tag_body, |(tag, record_fields)| {
    AstNodeImpl::new_record_builder(SourceLoc::new(&input), None, tag, record_fields)
  })(input.clone())
}

/// Parse a record field access expression, without a base, evaluates to record field index
pub(crate) fn parse_record_field_access_no_base(input: ParserInput) -> ParserResult<AstNode> {
  let mk_field_index =
    |(consumed_input, (tag, field)): (ParserInput, (String, String))| -> AstNode {
      AstNodeImpl::new_record_field(SourceLoc::new(&consumed_input), None, tag, field)
    };
  map(
    consumed(tuple((preceded(tok_hash, tok_atom), preceded(tok_period, tok_atom)))),
    mk_field_index,
  )(input)
}

// /// Parse a record field access expression with a base, evaluates to field value
// pub fn parse_record_field_access_with_base(input: ParserInput) -> ParserResult<AstNode> {
//   let mk_field_access =
//     |(consumed_input, (left, tag, field)): (ParserInput, (AstNode, String, String))| -> AstNode {
//       AstNodeImpl::new_record_field(SourceLoc::new(&consumed_input), Some(left), tag, field)
//     };
//   map(
//     consumed(tuple((
//       parse_expr_prec01,
//       preceded(tok_hash, tok_atom),
//       preceded(tok_period, tok_atom),
//     ))),
//     mk_field_access,
//   )(input)
// }
