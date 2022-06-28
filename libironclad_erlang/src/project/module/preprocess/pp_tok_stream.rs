//! Token source owning its tokens vector

use crate::erl_syntax::parsers::misc::panicking_parser_error_reporter;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::preprocessor::parsers::parse_pp::parse_preproc_directive;
use crate::erl_syntax::preprocessor::pp_node::PreprocessorNode;
use crate::erl_syntax::token_stream::token::Token;
use nom::Finish;

enum TokenStreamData<'a> {
  Owned(Vec<Token>),
  Borrowed(&'a [Token]),
}

/// Token stream which owns its tokens
pub struct TokenStream<'a> {
  /// The token stream
  tokens: TokenStreamData<'a>,
}

impl<'a> TokenStream<'a> {
  /// Access to contents
  pub fn as_slice(&self) -> &[Token] {
    match &self.tokens {
      TokenStreamData::Owned(o) => &o,
      TokenStreamData::Borrowed(b) => b,
    }
  }

  /// Invoke parser producing a preprocessor node
  pub fn parse_as_preprocessor(&self, original_input: &str) -> (ParserInput, PreprocessorNode) {
    let parser_input = ParserInput::new_slice(self.as_slice());
    let (tail, ppnode) = panicking_parser_error_reporter(
      original_input,
      parser_input.clone(),
      parse_preproc_directive(parser_input).finish(),
      true,
    );
    (tail, ppnode)
  }

  /// Create a new owned token stream
  pub fn new_owned(input: Vec<Token>) -> Self {
    TokenStream { tokens: TokenStreamData::Owned(input) }
  }

  /// Create a new borrowed token stream
  pub fn new_borrowed(input: &'a [Token]) -> Self {
    TokenStream { tokens: TokenStreamData::Borrowed(input) }
  }
}
