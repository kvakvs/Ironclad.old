//! Token array processing functions

use crate::erl_syntax::token_stream::token::Token;
use std::ptr::null;

/// Iterate over token stream lines, by finding `Newline` tokens.
/// Note: `Newline` tokens are removed in the preprocessor.
#[derive(Clone)]
pub(crate) struct TokenLinesIter<'a> {
  /// The array of tokens we use as input
  pub base: (*const Token, usize),
  /// Position of slice start as integer index into base array
  pub slice_start: usize,
  /// The length of the resulting slice
  pub slice_len: usize,
  _phantom: std::marker::PhantomData<&'a ()>,
}

impl<'a> Default for TokenLinesIter<'a> {
  fn default() -> Self {
    Self {
      base: (null(), 0),
      slice_start: 0,
      slice_len: 0,
      _phantom: Default::default(),
    }
  }
}

impl<'a> Iterator for TokenLinesIter<'a> {
  type Item = &'static [Token];

  /// Step forward and find next line, starting from end of previous slice (at `self.pos`)
  fn next(&mut self) -> Option<Self::Item> {
    if self.eof() {
      return None;
    }

    // Skip over the last produced slice to begin after it
    self.slice_start += self.slice_len;

    // Find next newline and return up to it
    for i in self.slice_start..self.base.1 {
      if self.read_base(i).is_eol() {
        // Found a newline, +1 to include it
        let result = self.slice_base(self.slice_start, i + 1 - self.slice_start);
        self.slice_len = result.len();
        return Some(result);
      }
    }

    // return everything that's remaining
    let all_result = self.slice_base_from(self.slice_start);
    self.slice_len = all_result.len();
    Some(all_result)
  }
}

impl<'a> TokenLinesIter<'a> {
  /// Access the `Token` at index `i`
  #[inline]
  fn read_base(&self, i: usize) -> &Token {
    unsafe { &*self.base.0.add(i) }
  }

  /// Return slice from position `from` of length `len`
  #[inline]
  fn slice_base(&self, from: usize, len: usize) -> &'static [Token] {
    unsafe { std::slice::from_raw_parts(self.base.0.add(from), len) }
  }

  /// Return remaining slice from position `from`
  #[inline]
  fn slice_base_from(&self, from: usize) -> &'static [Token] {
    unsafe { std::slice::from_raw_parts(self.base.0.add(from), self.base.1 - from) }
  }

  /// Create a new lines iterator for token stream, searching for `Newline` tokens
  pub fn new(base: (*const Token, usize)) -> Self {
    Self {
      base,
      slice_len: 0,
      slice_start: 0,
      _phantom: Default::default(),
    }
  }

  /// Override to a new token stream, in case when it needs to be modified while iterating.
  /// Example: Include files are pasted into the stream
  pub(crate) fn set_base(&mut self, new_base: &[Token]) {
    self.base = (new_base.as_ptr(), new_base.len());
  }

  /// Check whether we have reached the end of the input. Consider `pos` + `slice_len`, as the last
  /// returned slice.
  #[inline]
  pub fn eof(&self) -> bool {
    self.slice_start + self.slice_len >= self.base.1
  }

  #[inline]
  pub fn build_slice(&self) -> &'static [Token] {
    unsafe { std::slice::from_raw_parts(self.base.0.add(self.slice_start), self.slice_len) }
  }

  /// Take current slice and expand its end to the next newline
  pub fn expand_till_next_line(&mut self) -> Option<&'static [Token]> {
    if self.eof() {
      return None;
    }

    // Scan tokens after pos till we find a newline
    let scan_start = self.slice_start + self.slice_len;
    for i in scan_start..self.base.1 {
      if self.read_base(i).is_eol() {
        // Add 1 to include the newline
        self.slice_len = self.slice_len + i - scan_start + 1;
        let result = self.build_slice();
        // println!("itr: expand: {}", format_tok_stream(result, result.len()));
        return Some(result);
      }
    }
    None
  }
}
