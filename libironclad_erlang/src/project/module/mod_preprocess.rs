//! Preprocessing support for `ErlModule`

use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::preprocessor::parsers::preprocessor::parse_preproc_directive;
use crate::erl_syntax::token_stream::token::{format_tok_line, Token};
use crate::erl_syntax::token_stream::token_line_iter::TokenLinesIter;
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::error::ic_error::IcResult;
use crate::project::module::mod_impl::{ErlModule, ErlModuleImpl};
use nom::Finish;

impl ErlModuleImpl {
  /// Filter through the tokens array and produce a new token array with preprocessor directives
  /// eliminated, files included and macros substituted.
  pub fn preprocess(_module: &ErlModule, tokens: &[Token]) -> IcResult<Vec<Token>> {
    let mut result: Vec<Token> = Vec::with_capacity(tokens.len());
    let mut itr = TokenLinesIter::new(tokens);

    while let Some(line) = itr.next() {
      if line.len() > 2 && line[0].is_tok(TokenType::Minus) && line[1].is_atom() {
        // The line is a beginning of an attribute or a preprocessor definition or condition
        // These can only span one or more full lines, so we can work with lines iterator
        match parse_preproc_directive(ParserInput::new_slice(line)).finish() {
          Ok((tail, out)) => println!("PARSED: {} TAIL=«{}»", out, format_tok_line(tail.tokens)),
          Err(e) => println!("ERROR {:?}", e),
        }
      } else {
        // copy the line contents
        result.extend(line.iter().cloned())
      }
    }

    // TODO: Interpret -define/undef -if/ifdef/ifndef/else
    // TODO: Interpret -include and -include_lib
    // TODO: Parse and store other module attributes
    Ok(result)
  }
}
