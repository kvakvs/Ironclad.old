//! Inputs chaining for nested parsers

use crate::erl_syntax::parsers::misc::is_part_of;
use crate::source_file::SourceFile;
use std::sync::Arc;

/// Parser input slice carries the parent string and the offsets (start, end)
/// Slices are chained old to new, forming a readonly list of past inputs, allowing to track back,
/// and find the parsing locations
#[derive(Clone)]
pub struct ParserInputSlice {
  /// Maybe an `Arc<>` to the source file, if we have the filename.
  pub parent_file: Option<SourceFile>,
  /// The parent string where we are reading from, may contain more data than `input_slice`
  pub parent: Arc<String>,
  /// Where the parsing begins in the parent string
  pub input_start: usize,
  /// The length where the parsing ends.
  pub input_len: usize,
  /// The read position for Nom parsers
  pub read_pos: usize,
  /// Forming a readonly list of parse history
  pub prev: Option<Arc<ParserInputSlice>>,
}

impl ParserInputSlice {
  /// Create a new parser input slice. Use `ParserInput::from_str` instead
  pub(crate) fn new(text: &str) -> Self {
    let text_as_str = Arc::new(text.to_string());
    Self {
      parent_file: None,
      parent: text_as_str,
      input_start: 0,
      read_pos: 0,
      input_len: text.len(),
      prev: None,
    }
  }

  /// Create and chain a new parser input slice.
  pub(crate) fn chain_into_new(
    current: &Arc<ParserInputSlice>,
    text: &str,
  ) -> Arc<ParserInputSlice> {
    let text_as_str = Arc::new(text.to_string());
    Self {
      parent_file: current.parent_file.clone(),
      parent: text_as_str,
      input_start: 0,
      read_pos: 0,
      input_len: text.len(),
      prev: Some(current.clone()),
    }
    .into()
  }

  /// Guarantees are on the programmer to create slice which belongs to the valid string
  pub(crate) fn clone_with_read_slice(&self, new_input: &str) -> Arc<Self> {
    let parent_str = self.parent.as_str();
    assert!(
      is_part_of(parent_str, new_input),
      "Cloning input slice is only allowed with a slice in the same string"
    );
    let input_start = new_input.as_ptr() as usize - parent_str.as_ptr() as usize;
    Self {
      parent_file: self.parent_file.clone(),
      parent: self.parent.clone(),
      input_start,
      read_pos: input_start,
      input_len: new_input.len(),
      prev: self.prev.clone(),
    }
    .into()
  }

  /// Nested parsing of a new file
  pub(crate) fn new_with_source_file(file: SourceFile) -> Arc<Self> {
    Self {
      parent_file: Some(file.clone()),
      parent: file.text.clone(),
      input_start: 0,
      read_pos: 0,
      input_len: file.text.len(),
      prev: None, // no previous?
    }
    .into()
  }

  /// Quick access to content read window as `&str`
  #[inline(always)]
  pub(crate) fn as_str<'s>(&self) -> &'s str {
    unsafe {
      std::str::from_utf8_unchecked(std::slice::from_raw_parts(
        self.parent.as_ptr().add(self.read_pos),
        self.input_len,
      ))
    }
  }
}

impl std::fmt::Display for ParserInputSlice {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.as_str())
  }
}

impl std::fmt::Debug for ParserInputSlice {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "InputSlice[ «{}» ]", self.as_str())
  }
}
