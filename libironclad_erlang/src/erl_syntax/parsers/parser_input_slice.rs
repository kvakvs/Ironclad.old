//! Inputs chaining for nested parsers

use crate::source_file::SourceFile;
use std::sync::Arc;

/// Parser input slice carries the parent string and the offsets (start, end)
/// Slices are chained old to new, forming a readonly list of past inputs, allowing to track back,
/// and find the parsing locations
#[derive(Debug, Clone)]
pub struct ParserInputSlice {
  pub parent_file: Option<SourceFile>,
  /// The parent string where we are reading from
  pub parent: Arc<String>,
  /// Not entire parent is used, but a part of it
  pub input_slice: &'static str,
  /// The read pointer for Nom parsers
  pub read_pointer: &'static str,
  /// Length of inputs so far, before this input slice
  pub prev_length: usize,
  /// Forming a readonly list of parse history
  pub prev: Arc<ParserInputSlice>,
}
