//! Groups type definitions shared by all preprocessor parse modules
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{period_newline, tok, tok_atom_of, tok_string};
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::preprocessor::parsers::def_undef::{define_directive, undef_directive};
use crate::erl_syntax::preprocessor::parsers::if_ifdef::{
  elif_directive, else_directive, endif_directive, if_directive, ifdef_directive, ifndef_directive,
  tok_macro_ident,
};
use crate::erl_syntax::preprocessor::parsers::parse_attr::parse_module_attr;
use crate::erl_syntax::preprocessor::pp_node::pp_impl::PreprocessorNodeImpl;
use crate::erl_syntax::preprocessor::pp_node::PreprocessorNode;
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::source_loc::SourceLoc;
use nom::branch::alt;
use nom::combinator::map;
use nom::error::context;
use nom::multi::separated_list0;
use nom::sequence::delimited;

/// Parse a `Macroident1, Macroident2, ...` into a list
pub(crate) fn comma_sep_macro_idents(input: ParserInput) -> ParserResult<Vec<String>> {
  separated_list0(tok(TokenType::Comma), tok_macro_ident)(input)
}

// /// Parse an identifier, starting with a letter and also can be containing numbers and underscoress
// pub(crate) fn macro_ident(input: ParserInput) -> ParserResult<String> {
//   map(
//     recognize(pair(
//       verify(anychar, |c: Token| c.is_alphabetic() || *c == '_'),
//       many0(alt((alphanumeric1, tag("_".into())))),
//     )),
//     |pi| pi.to_string(),
//   )(input)
// }

/// Parse a `-include(STRING)`
fn include_directive(input: ParserInput) -> ParserResult<PreprocessorNode> {
  match delimited(
    tok_atom_of("include"),
    delimited(tok(TokenType::ParOpen), tok_string, tok(TokenType::ParClose)),
    period_newline,
  )(input.clone())
  {
    Ok((input2, path)) => {
      // let included_file = input
      //   .scope
      //   .load_include(SourceLoc::new(&input), &PathBuf::from(path), input.file_name())
      //   .unwrap();
      // let input3 = input2.clone_with_input(included_file);
      // tokenize_source(input3)
      let node =
        PreprocessorNodeImpl::new_include(SourceLoc::new(&input), path.as_str().to_string());
      Ok((input2, node))
    }
    Err(result) => Err(result),
  }
}

/// Parse a `-include_lib(STRING)`
fn include_lib_directive(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(
    delimited(
      tok_atom_of("include_lib"),
      delimited(tok(TokenType::ParOpen), tok_string, tok(TokenType::ParClose)),
      period_newline,
    ),
    |t| PreprocessorNodeImpl::new_include_lib(SourceLoc::new(&input), t.as_str().to_string()),
  )(input.clone())
}

/// Parse a `-error(STRING)`
fn error_directive(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(
    delimited(
      tok_atom_of("error"),
      delimited(tok(TokenType::ParOpen), tok_string, tok(TokenType::ParClose)),
      period_newline,
    ),
    |t| PreprocessorNodeImpl::new_error(SourceLoc::new(&input), t.as_str().to_string()),
  )(input.clone())
}

/// Parse a `-warning(STRING)`
fn warning_directive(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(
    delimited(
      tok_atom_of("warning"),
      delimited(tok(TokenType::ParOpen), tok_string, tok(TokenType::ParClose)),
      period_newline,
    ),
    |t| PreprocessorNodeImpl::new_warning(SourceLoc::new(&input), t.as_str().to_string()),
  )(input.clone())
}

/// Parse one of supported preprocessor directives
pub(crate) fn parse_preproc_directive(input: ParserInput) -> ParserResult<PreprocessorNode> {
  alt((
    // -define is special, it needs closing ).\n to consume the content
    context("'-define()' directive", define_directive),
    context("'-undef()' directive", undef_directive),
    // temporary nodes used by parse_if_block
    context("'-endif()' directive", endif_directive),
    context("'-elif()' directive", elif_directive),
    context("'-else()' directive", else_directive),
    context("'-ifdef()' directive", ifdef_directive),
    context("'-ifndef()' directive", ifndef_directive),
    context("'-if()' directive", if_directive),
    context("'-warning()' directive", warning_directive),
    context("'-error()' directive", error_directive),
    context("'-include_lib()' directive", include_lib_directive),
    context("'-include()' directive", include_directive),
    parse_module_attr,
  ))(input)
}
