//! Groups type definitions shared by all preprocessor parse modules
// use crate::erl_syntax::erl_ast::AstNode;
// use crate::erl_syntax::parsers::defs::ParserResult;
// use crate::erl_syntax::parsers::misc::{period_newline, tok, tok_atom_of, tok_string};
// use crate::erl_syntax::parsers::parse_module;
// use crate::erl_syntax::parsers::parser_input::ParserInput;
// use crate::erl_syntax::preprocessor::ast::PreprocessorNodeType;
// use crate::erl_syntax::preprocessor::parsers::if_ifdef::{
//   elif_temporary_directive, else_temporary_directive, endif_temporary_directive, ifdef_directive,
//   ifndef_temporary_directive, parse_if_block,
// };
// use crate::erl_syntax::token_stream::token_type::TokenType;
// use crate::source_loc::SourceLoc;
// use nom::branch::alt;
// use nom::bytes::complete::tag;
// use nom::character::complete::{alphanumeric1, anychar};
// use nom::combinator::{cut, map, recognize, verify};
// use nom::error::context;
// use nom::multi::{many0, separated_list0};
// use nom::sequence::{delimited, pair, tuple};
// use std::path::PathBuf;
//
// /// Parse a `Macroident1, Macroident2, ...` into a list
// pub(crate) fn comma_sep_macro_idents(input: ParserInput) -> ParserResult<Vec<String>> {
//   separated_list0(tok(TokenType::Comma), macro_ident)(input)
// }
//
// /// Parse an identifier, starting with a letter and also can be containing numbers and underscoress
// pub(crate) fn macro_ident(input: ParserInput) -> ParserResult<String> {
//   map(
//     recognize(pair(
//       verify(anychar, |c: &char| c.is_alphabetic() || *c == '_'),
//       many0(alt((alphanumeric1, tag("_".into())))),
//     )),
//     |pi| pi.to_string(),
//   )(input)
// }
//
// /// Parse a `-include(STRING)`
// fn include_directive(input: ParserInput) -> ParserResult<AstNode> {
//   let result = delimited(
//     tok_atom_of("include"),
//     delimited(tok(TokenType::ParOpen), tok_string, tok(TokenType::ParClose)),
//     period_newline,
//   )(input.clone());
//
//   if let Ok((input2, path)) = result {
//     let included_file = input
//       .parser_scope
//       .load_include(SourceLoc::new(&input), &PathBuf::from(path), input.file_name())
//       .unwrap();
//     let input3 = input2.clone_with_input(included_file);
//     parse_module(input3)
//   } else {
//     Err(result.unwrap_err())
//   }
// }
//
// /// Parse a `-include_lib(STRING)`
// fn include_lib_directive(input: ParserInput) -> ParserResult<AstNode> {
//   map(
//     delimited(
//       tok_atom_of("include_lib"),
//       delimited(tok(TokenType::ParOpen), tok_string, tok(TokenType::ParClose)),
//       period_newline,
//     ),
//     |t| PreprocessorNodeType::new_include_lib(SourceLoc::new(&input), t.into_string()),
//   )(input.clone())
// }
//
// /// Parse a `-error(STRING)`
// fn error_directive(input: ParserInput) -> ParserResult<AstNode> {
//   map(
//     delimited(
//       tok_atom_of("error"),
//       delimited(tok(TokenType::ParOpen), tok_string, tok(TokenType::ParClose)),
//       period_newline,
//     ),
//     |t| PreprocessorNodeType::new_error(SourceLoc::new(&input), t.into_string()),
//   )(input.clone())
// }
//
// /// Parse a `-warning(STRING)`
// fn warning_directive(input: ParserInput) -> ParserResult<AstNode> {
//   map(
//     delimited(
//       tok_atom_of("warning"),
//       delimited(tok(TokenType::ParOpen), tok_string, tok(TokenType::ParClose)),
//       period_newline,
//     ),
//     |t| PreprocessorNodeType::new_warning(SourceLoc::new(&input), t.into_string()),
//   )(input.clone())
// }
//
// /// Parse one of supported preprocessor directives
// pub(crate) fn parse_preproc_directive(input: ParserInput) -> ParserResult<AstNode> {
//   alt((
//     // -define is special, it needs closing ).\n to consume the content
//     context("'-define()' directive", define_directive),
//     context("'-undef()' directive", undef_directive),
//     // temporary nodes used by parse_if_block
//     context("'-endif()' directive", endif_temporary_directive),
//     context("'-elif()' directive", elif_temporary_directive),
//     context("'-else()' directive", else_temporary_directive),
//     context("'-ifdef()' directive", ifdef_directive),
//     context("'-ifndef()' directive", ifndef_temporary_directive),
//     context("'-if()' directive", parse_if_block), // if must go after longer words ifdef and ifndef
//     context("'-warning()' directive", warning_directive),
//     context("'-error()' directive", error_directive),
//     context("'-include_lib()' directive", include_lib_directive),
//     context("'-include()' directive", include_directive),
//   ))(input)
// }
