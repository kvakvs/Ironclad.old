//! Token array processing functions

use crate::erl_syntax::token_stream::token::Token;

/// Iterate over token stream lines, by finding `Newline` tokens.
/// Note: `Newline` tokens are removed in the preprocessor.
pub(crate) struct TokLinesIter<'a> {
  pub base: &'a [Token],
  pub pos: usize,
}

impl<'a> TokLinesIter<'a> {
  /// Create a new lines iterator for token stream, searching for `Newline` tokens
  pub fn new(base: &'a [Token]) -> Self {
    Self { base, pos: 0 }
  }
}

impl<'a> Iterator for TokLinesIter<'a> {
  type Item = &'a [Token];

  fn next(&mut self) -> Option<Self::Item> {
    if self.pos >= self.base.len() {
      return None;
    }

    // Find next newline and return up to it
    for i in self.pos..self.base.len() {
      if self.base[i].is_newline() {
        let result = &self.base[self.pos..i];
        self.pos = i + 1;
        return Some(result);
      }
    }
    // return everything that's remaining
    let all_result = &self.base[self.pos..];
    self.pos = self.base.len();
    Some(all_result)
  }
}
