//! Parsing tools for `-if` family of directives

use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::literal_bool::LiteralBool;
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{dash_atom, period_eol_eof, tok_atom, tok_atom_of, tok_var};
use crate::erl_syntax::parsers::misc_tok::*;
use crate::erl_syntax::parsers::parse_expr::{parse_constant_expr, parse_expr};
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::preprocessor::pp_node::pp_impl::PreprocessorNodeImpl;
use crate::erl_syntax::preprocessor::pp_node::pp_type::PreprocessorNodeType;
use crate::erl_syntax::preprocessor::pp_node::PreprocessorNode;
use crate::source_loc::SourceLoc;
use nom::branch::alt;
use nom::combinator::{cut, map, opt, recognize};
use nom::error::context;
use nom::sequence::{delimited, pair, preceded, terminated};

// /// Parses multiple lines of any directives except `-endif.` or `-else.`
// fn parse_fragments_till_else(input: ParserInput) -> VecAstParserResult {
//   many0(verify(parse_one_module_form, |frag: &AstNode| {
//     !frag.is_else() && !frag.is_elseif() && !frag.is_endif()
//   }))(input)
// }

// /// Parses multiple lines of any directives except `-endif.`
// fn parse_fragments_till_endif(input: ParserInput) -> VecAstParserResult {
//   many0(verify(parse_one_module_form, |frag: &AstNode| !frag.is_endif()))(input)
// }

// /// Parse a `-if(EXPR).` `<LINES>` then optional `-else. <LINES> -endif.`
// pub fn parse_if_block(input: ParserInput) -> ParserResult<PreprocessorNode> {
//   map(
//     terminated(
//       tuple((
//         if_condition,
//         // Consume lines and directives until an `-else` or `-endif`
//         context("condition true section of a -if()", parse_fragments_till_else),
//         // Optional -else. <LINES> block
//         context(
//           "condition false section of a -if()",
//           opt(preceded(else_temporary_directive, parse_fragments_till_endif)),
//         ),
//       )),
//       // Ending with an endif
//       endif_temporary_directive,
//     ),
//     |(if_cond, branch_true, branch_false)| {
//       if if_cond {
//         PreprocessorNode::new_group_node_temporary(branch_true)
//       } else {
//         PreprocessorNode::new_group_node_temporary(branch_false.unwrap_or_default())
//       }
//     },
//   )(input.clone())
// }

/// Parse a `-if(EXPR).\n` and return a temporary node
pub fn if_condition(input: ParserInput) -> ParserResult<bool> {
  map(
    delimited(keyword_if, delimited(tok_par_open, parse_expr, tok_par_close), period_eol_eof),
    // Builds a temporary If node with erl expression in it
    |expr| match expr.walk_boolean_litexpr() {
      LiteralBool::False => false,
      LiteralBool::True => true,
      LiteralBool::NotABoolean => panic!("Bool expression is expected here"),
    },
  )(input.clone())
}

/// Parse a `-elif(EXPR)` into a temporary AST node
pub(crate) fn elif_directive(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(
    delimited(
      |i1| dash_atom(i1, "elif"),
      delimited(tok_par_open, parse_expr, tok_par_close),
      period_eol_eof,
    ),
    |t| PreprocessorNodeImpl::new_elif(SourceLoc::new(&input), t),
  )(input.clone())
}

/// Macro ident can match atom or variable name rules
#[inline]
pub(crate) fn tok_macro_ident(input: ParserInput) -> ParserResult<String> {
  alt((tok_atom, tok_var))(input)
}

pub(crate) fn ifdef_directive(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(
    delimited(
      |i1| dash_atom(i1, "ifdef"),
      delimited(tok_par_open, tok_macro_ident, tok_par_close),
      period_eol_eof,
    ),
    |tag: String| PreprocessorNodeImpl::new_ifdef(SourceLoc::new(&input), tag),
  )(input.clone())
}

pub(crate) fn if_directive(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(
    preceded(
      pair(tok_minus, alt((keyword_if, tok_atom_of("if")))),
      context(
        "-if() directive",
        cut(terminated(
          delimited(tok_par_open, parse_constant_expr, tok_par_close),
          period_eol_eof,
        )),
      ),
    ),
    |expr: AstNode| PreprocessorNodeImpl::new_if(SourceLoc::new(&input), expr),
  )(input.clone())
}

// /// Parse a `-ifdef(MACRO_NAME)`
// pub(crate) fn ifdef_directive(input: ParserInput) -> ParserResult<PreprocessorNode> {
//   map(
//     terminated(
//       tuple((
//         ifdef_condition,
//         context("condition true section of an -ifdef()", parse_fragments_till_else),
//         context(
//           "condition false section of an -ifdef()",
//           opt(preceded(else_temporary_directive, parse_fragments_till_endif)),
//         ),
//       )),
//       endif_temporary_directive,
//     ),
//     |(cond_true, branch_true, branch_false)| {
//       PreprocessorNode::new_group_node_temporary(if cond_true {
//         branch_true
//       } else {
//         branch_false.unwrap_or_default()
//       })
//     },
//   )(input.clone())
// }

/// Parse a `-ifndef(MACRO_NAME)`
pub(crate) fn ifndef_directive(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(
    delimited(
      tok_atom_of("ifndef"),
      delimited(tok_par_open, tok_macro_ident, tok_par_close),
      period_eol_eof,
    ),
    |t: String| PreprocessorNodeImpl::new_ifndef(SourceLoc::new(&input), t),
  )(input.clone())
}

/// Parse a `-else.`, return a temporary `Else` node, which will not go into final `PpAst`
pub(crate) fn else_directive(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(
    preceded(
      pair(tok_minus, alt((keyword_else, tok_atom_of("else")))),
      context(
        "-else() directive",
        cut(terminated(opt(pair(tok_par_open, tok_par_close)), period_eol_eof)),
      ),
    ),
    |_opt| {
      PreprocessorNodeImpl::new_with_location(SourceLoc::new(&input), PreprocessorNodeType::Else)
    },
  )(input.clone())
}

fn maybe_empty_parens(input: ParserInput) -> ParserResult<ParserInput> {
  recognize(opt(pair(tok_par_open, tok_par_close)))(input)
}

/// Parse a `-endif.`, return a temporary `Endif` node, which will not go into final `PpAst`
pub(crate) fn endif_directive(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(
    delimited(|i1| dash_atom(i1, "endif"), maybe_empty_parens, period_eol_eof),
    |_opt| {
      PreprocessorNodeImpl::new_with_location(SourceLoc::new(&input), PreprocessorNodeType::Endif)
    },
  )(input.clone())
}
