//! Token array processing functions

use crate::erl_syntax::token_stream::token::Token;

/// Iterate over token stream lines, by finding `Newline` tokens.
/// Note: `Newline` tokens are removed in the preprocessor.
pub(crate) struct TokenLinesIter<'a> {
  /// The array of tokens we use as input
  pub base: &'a [Token],
  /// Position of slice start as integer index into base array
  pub pos: usize,
  /// The length of the resulting slice
  pub slice_len: usize,
}

impl<'a> TokenLinesIter<'a> {
  /// Create a new lines iterator for token stream, searching for `Newline` tokens
  pub fn new(base: &'a [Token]) -> Self {
    Self { base, slice_len: 0, pos: 0 }
  }
}

impl<'a> Iterator for TokenLinesIter<'a> {
  type Item = &'a [Token];

  fn next(&mut self) -> Option<Self::Item> {
    if self.eof() {
      return None;
    }

    // Skip over the last produced slice to begin after it
    self.pos = self.pos + self.slice_len;

    // Find next newline and return up to it
    for i in self.pos..self.base.len() {
      if self.base[i].is_newline() {
        // Found a newline
        let result = &self.base[self.pos..i + 1];
        self.slice_len = result.len();
        return Some(result);
      }
    }

    // return everything that's remaining
    let all_result = &self.base[self.pos..];
    self.slice_len = all_result.len();
    Some(all_result)
  }
}

impl<'a> TokenLinesIter<'a> {
  /// Check whether we have reached the end of the input. Consider `pos` + `slice_len`, as the last
  /// returned slice.
  #[inline]
  pub fn eof(&self) -> bool {
    self.pos + self.slice_len >= self.base.len()
  }

  #[inline]
  pub fn build_slice(&self) -> &'a [Token] {
    &self.base[self.pos..self.pos + self.slice_len]
  }

  /// Take current slice and expand its end to the next newline
  pub fn expand_till_next_line(&mut self) -> Option<&'a [Token]> {
    if self.eof() {
      return None;
    }

    // Scan tokens after pos till we find a newline
    let scan_start = self.pos + self.slice_len;
    for i in scan_start..self.base.len() {
      if self.base[i].is_newline() {
        self.slice_len = self.slice_len + i - scan_start;
        return Some(self.build_slice());
      }
    }
    None
  }
}
