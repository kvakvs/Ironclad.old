//! Format parse errors

use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::token_stream::token::{format_tok_line, format_tok_stream};
use crate::erl_syntax::token_stream::token_array::TokLinesIter;
use crate::erl_syntax::token_stream::token_type::TokenType;

/// Transforms a `VerboseError` into a trace with input position information
/// Copy from `nom::error::convert_error` to support token stream `&[Token]` errors.
pub fn convert_token_stream_parser_error(
  _original_input: &str,
  tokens_input: ParserInput,
  e: nom::error::VerboseError<ParserInput>,
) -> String {
  // TODO: Same treatment for tokenizer errors, which also call nom's convert_error
  use nom::Offset;
  use std::fmt::Write;

  let mut result = String::new();

  for (i, (substring, kind)) in e.errors.iter().enumerate() {
    let inp_offset = tokens_input.offset(substring);

    if tokens_input.is_empty() {
      match kind {
        nom::error::VerboseErrorKind::Char(c) => {
          write!(&mut result, "{}: expected '{}', got empty input\n\n", i, c)
        }
        nom::error::VerboseErrorKind::Context(s) => {
          write!(&mut result, "{}: in {}, got empty input\n\n", i, s)
        }
        nom::error::VerboseErrorKind::Nom(e) => {
          write!(&mut result, "{}: in {:?}, got empty input\n\n", i, e)
        }
      }
    } else {
      let prefix = &tokens_input.tokens[..inp_offset];

      // Count the number of newlines in the first `offset` bytes of input
      let line_number = prefix
        .iter()
        .filter(|&b| matches!(b.content, TokenType::Newline))
        .count()
        + 1;

      // Find the line that includes the subslice:
      // Find the *last* newline before the substring starts
      let line_begin = prefix
        .iter()
        .rev()
        .position(|b| matches!(b.content, TokenType::Newline))
        .map(|pos| inp_offset - pos)
        .unwrap_or(0);

      // Find the full line after that newline
      let line = TokLinesIter::new(&tokens_input.tokens[line_begin..])
        .next()
        .unwrap_or(&tokens_input.tokens[line_begin..]);
      // .trim_end();

      // The (1-indexed) column number is the offset of our substring into that line
      let column_number: usize = substring.offset_inside(line) + 1;

      match kind {
        nom::error::VerboseErrorKind::Char(c) => {
          if let Some(actual) = substring.tokens.iter().next() {
            write!(
              &mut result,
              "{i}: at line {line_number}:{column}:\n\
               {line}\n\
               expected '{expected}', found {actual}\n\n",
              i = i,
              line_number = line_number,
              line = format_tok_line(line),
              column = column_number,
              expected = c,
              actual = actual,
            )
          } else {
            write!(
              &mut result,
              "{i}: at line {line_number}:{column}:\n\
               {line}\n\
               expected '{expected}', got end of input\n\n",
              i = i,
              line_number = line_number,
              line = format_tok_line(line),
              column = column_number,
              expected = c,
            )
          }
        }
        nom::error::VerboseErrorKind::Context(s) => write!(
          &mut result,
          "{i}: at line {line_number}:{column}, in {context}:\n\
           {line}\n",
          i = i,
          line_number = line_number,
          column = column_number,
          context = s,
          line = format_tok_line(line),
        ),
        nom::error::VerboseErrorKind::Nom(e) => write!(
          &mut result,
          "{i}: at line {line_number}:{column}, in {nom_err:?}:\n\
           {line}\n",
          i = i,
          line_number = line_number,
          nom_err = e,
          line = format_tok_line(line),
          column = column_number,
        ),
      }
    }
    // Because `write!` to a `String` is infallible, this `unwrap` is fine.
    .unwrap();
  }

  result
}
