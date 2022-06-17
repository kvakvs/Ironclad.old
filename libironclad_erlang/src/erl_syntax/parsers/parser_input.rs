//! Contains implementations required for custom nom input to work

use crate::erl_syntax::parsers::misc::is_part_of;
use crate::erl_syntax::parsers::parser_scope::{ParserScope, ParserScopeImpl};
use crate::erl_syntax::token_stream::token::Token;
use crate::source_file::SourceFile;
use crate::source_loc::SourceLoc;
use nom::{CompareResult, Needed};
use std::iter::{Copied, Enumerate, Map};
use std::mem::size_of;
use std::ops::{Deref, RangeFrom, RangeTo};
use std::path::PathBuf;
use std::slice::Iter;
use std::str::{CharIndices, Chars, SplitTerminator};
use std::sync::Arc;

/// The nom-compatible token input
#[derive(Debug, Clone)]
pub struct ParserInput<'a> {
  /// Access to filename and source text, if value is defined
  pub source_file: Option<SourceFile>,
  /// The token stream
  pub tokens: &'a [Token],
}

impl<'a> nom::Offset for ParserInput<'a> {
  fn offset(&self, second: &Self) -> usize {
    let fst = self.tokens.as_ptr();
    let snd = second.tokens.as_ptr();

    (snd as usize - fst as usize) / size_of::<Token>()
  }
}

// impl std::fmt::Display for ParserInputImpl<'_> {
//   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//     write!(f, "{}", self.as_str())
//   }
// }
//
// impl std::fmt::Debug for ParserInputImpl<'_> {
//   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//     write!(f, "ParserInput[ scope={:?}, input={:?} ]", &self.parser_scope, self.input)
//   }
// }
//

impl<'a> ParserInput<'a> {
  /// Calculates offset for second inside `self`
  pub(crate) fn offset_inside(&self, second: &[Token]) -> usize {
    let fst = self.tokens.as_ptr();
    let snd = second.as_ptr();
    assert!(snd > fst);

    (snd as usize - fst as usize) / size_of::<Token>()
  }

  pub(crate) fn new(source_file: &SourceFile, tokens: &'a [Token]) -> Self {
    Self { source_file: Some(source_file.clone()), tokens }
  }

  pub(crate) fn is_empty(&self) -> bool {
    self.tokens.is_empty()
  }

  //   /// Return a code location
  //   pub(crate) fn loc(&self) -> SourceLoc {
  //     SourceLoc::from_input(self.input)
  //   }
  //
  //   /// Create a parser input with a string slice
  //   pub fn new(source_file: &SourceFile, input: &'a [Token]) -> Self {
  //     Self {
  //       parent_file: Some(source_file.clone()),
  //       parser_scope: ParserScopeImpl::new_empty().into(),
  //       input,
  //     }
  //   }
  //
  //   pub(crate) fn file_name(&self) -> Option<PathBuf> {
  //     self.parent_file.map(|pf| pf.file_name.to_path_buf())
  //     // if let Some(pf) = &self.input.parent_file {
  //     //   Some(pf.file_name.to_path_buf())
  //     // } else {
  //     //   None
  //     // }
  //   }
  //
  /// Clone into a new custom parser input from a str slice. Assert that it belongs to the same input slice.
  pub(crate) fn clone_with_slice(&self, input: &'a [Token]) -> Self {
    Self { source_file: None, tokens: input }
  }
  //
  //   /// Build a new custom parser input from a loaded source file
  //   pub(crate) fn new_with_scope(
  //     scope: ParserScope,
  //     source_file: &SourceFile,
  //     input: &'a [Token],
  //   ) -> Self {
  //     Self {
  //       parent_file: Some(source_file.clone()),
  //       parser_scope: scope,
  //       input,
  //     }
  //   }
  //
  //   /// Build a new custom parser input from a loaded source file
  //   pub(crate) fn clone_with_input(&self, input: &'a [Token]) -> Self {
  //     Self {
  //       parent_file: self.parent_file.clone(),
  //       parser_scope: self.parser_scope.clone(),
  //       input,
  //     }
  //   }
  //
  //   // /// Build a new custom parser and chain the old to it
  //   // pub(crate) fn clone_nested(&self, input: &str) -> Self {
  //   //   // println!("Parser input clone nested...");
  //   //   ParserInputImpl {
  //   //     parser_scope: self.parser_scope.clone(),
  //   //     input: ParserInputSlice::chain_into_new(&self.input, input),
  //   //     _phantom: Default::default(),
  //   //   }
  //   // }
  //
  //   /// Check whether there's any input remaining
  //   pub(crate) fn is_empty(&self) -> bool {
  //     self.as_str().is_empty()
  //   }
  //
  //   // /// Quick access to last input in chain as `&str`
  //   // #[inline(always)]
  //   // pub fn as_str(&self) -> &'a str {
  //   //   self.input.as_str()
  //   // }
}
//
// impl From<&str> for ParserInputImpl<'_> {
//   fn from(s: &str) -> Self {
//     ParserInputImpl::new_str(s)
//   }
// }
//
// impl nom::Offset for ParserInputImpl<'_> {
//   fn offset(&self, second: &Self) -> usize {
//     // Compare that chain of slices matches in both `self` and `second` and compare that the input
//     // string is the same input string in both.
//     // TODO: It is possible to implement correct offset inside virtual chain of inputs
//     assert_eq!(
//       self.input.parent.as_ptr(),
//       second.input.parent.as_ptr(),
//       "nom::Offset for unrelated slices not implemented (but possible!)"
//     );
//     let self_n = self.as_str().as_ptr() as usize;
//     let second_n = second.as_str().as_ptr() as usize;
//     // println!("Offset for {:x} vs {:x}", self_n, second_n);
//     assert!(
//       second_n >= self_n,
//       "Second input pointer must be greater than the first, when calculating nom::Offset"
//     );
//     second_n - self_n
//   }
// }

// impl Deref for ParserInput<'_> {
//   type Target = str;
//
//   fn deref(&self) -> &Self::Target {
//     self.tokens.iter().next().unwrap()
//   }
// }

impl nom::Slice<RangeFrom<usize>> for ParserInput<'_> {
  fn slice(&self, mut range: RangeFrom<usize>) -> Self {
    self.clone_with_slice(self.tokens.slice(range))
  }
}

impl nom::Slice<RangeTo<usize>> for ParserInput<'_> {
  fn slice(&self, range: RangeTo<usize>) -> Self {
    self.clone_with_slice(self.tokens.slice(range))
  }
}

impl<'a> nom::InputIter for ParserInput<'a> {
  type Item = Token;
  type Iter = Enumerate<Self::IterElem>;
  type IterElem = std::iter::Cloned<Iter<'a, Token>>;

  #[inline]
  fn iter_indices(&self) -> Self::Iter {
    self.iter_elements().enumerate()
  }
  #[inline]
  fn iter_elements(&self) -> Self::IterElem {
    self.tokens.iter().cloned()
  }
  #[inline]
  fn position<P>(&self, predicate: P) -> Option<usize>
  where
    P: Fn(Self::Item) -> bool,
  {
    self.tokens.iter().position(|b| predicate(b.clone()))
  }
  #[inline]
  fn slice_index(&self, count: usize) -> Result<usize, Needed> {
    if self.tokens.len() >= count {
      Ok(count)
    } else {
      Err(Needed::new(count - self.tokens.len()))
    }
  }
}

impl<'a> nom::InputLength for ParserInput<'a> {
  fn input_len(&self) -> usize {
    self.tokens.len()
  }
}
