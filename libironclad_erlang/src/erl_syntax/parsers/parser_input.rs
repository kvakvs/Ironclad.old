//! Contains implementations required for custom nom input to work

use crate::erl_syntax::parsers::misc::is_part_of;
use crate::erl_syntax::parsers::parser_input_slice::ParserInputSlice;
use nom::{CompareResult, Needed};
use std::ops::{Deref, RangeFrom, RangeTo};
use std::str::{CharIndices, Chars};
use std::sync::Arc;

// pub type ParserInput = &'a str;

/// Used as input to all parsers, and contains the chain of inputs (for nested parsing), and current
/// position for the current parser.
#[derive(Debug, Clone)]
pub struct CustomParserInput {
  /// Chain of inputs. Each input has an input string, an input range and a read pointer inside
  /// it. When an input is nested inside another input, a new input slice is added to the input
  /// chain using `prev_slice` field to bind them together.
  pub input: Arc<ParserInputSlice>,
}

impl std::fmt::Display for CustomParserInput {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "«{}»", self.as_str())
  }
}

impl CustomParserInput {
  /// Build a new custom parser input from a str slice. Assert that it belongs to the same input slice.
  pub fn clone_with_read_slice(&self, slice: &'static str) -> Self {
    assert!(
      is_part_of(self.input.input_slice, slice),
      "Input slice when cloning must belong to the last active input string"
    );
    Self { input: self.input.clone_with_read_slice(slice) }
  }

  /// Check whether there's any input remaining
  pub fn is_empty(&self) -> bool {
    self.as_str().is_empty()
  }

  /// Quick access to last input in chain as `&str`
  #[inline(always)]
  pub fn as_str(&self) -> &str {
    self.input.read_pointer
  }
}

// impl ToString for CustomParserInput {
//   fn to_string(&self) -> String {
//     self.as_str().to_string()
//   }
// }

impl nom::Offset for CustomParserInput {
  fn offset(&self, second: &Self) -> usize {
    // Compare that chain of slices matches in both `self` and `second` and compare that the input
    // string is the same input string in both.
    // TODO: It is possible to implement correct offset inside virtual chain of inputs
    assert!(
      self.input.prev_length == second.input.prev_length
        && self.input.input_slice.as_ptr() == second.input.input_slice.as_ptr(),
      "nom::Offset for unrelated slices not implemented (but possible!)"
    );
    assert!(
      second.as_str().as_ptr() > self.as_str().as_ptr(),
      "Second input pointer must be greater than the first, when calculating nom::Offset"
    );
    (second.as_str().as_ptr() as usize) - (self.as_str().as_ptr() as usize)
  }
}

impl Deref for CustomParserInput {
  type Target = str;

  fn deref(&self) -> &Self::Target {
    self.as_str()
  }
}

impl nom::Slice<RangeFrom<usize>> for CustomParserInput {
  fn slice(&self, range: RangeFrom<usize>) -> Self {
    self.clone_with_read_slice(self.input.input_slice.slice(range))
  }
}

impl nom::Slice<RangeTo<usize>> for CustomParserInput {
  fn slice(&self, range: RangeTo<usize>) -> Self {
    self.clone_with_read_slice(self.input.input_slice.slice(range))
  }
}

impl ParserInputSlice {
  /// Guarantees are on the programmer to create slice which belongs to the valid string
  fn clone_with_read_slice(&self, new_read_pointer: &'static str) -> Arc<Self> {
    Self {
      parent_file: self.parent_file.clone(),
      parent: self.parent.clone(),
      input_slice: self.input_slice,
      read_pointer: new_read_pointer,
      prev_length: self.prev_length,
      prev: self.prev.clone(),
    }
    .into()
  }
}

// Copied from impl for `nom::InputIter` for `&'a str` and adapted to handle last input
impl nom::InputIter for CustomParserInput {
  type Item = char;
  type Iter = CharIndices<'static>;
  type IterElem = Chars<'static>;

  #[inline]
  fn iter_indices(&self) -> Self::Iter {
    self.as_str().char_indices()
  }

  #[inline]
  fn iter_elements(&self) -> Self::IterElem {
    self.as_str().chars()
  }

  fn position<P>(&self, predicate: P) -> Option<usize>
  where
    P: Fn(Self::Item) -> bool,
  {
    for (o, c) in self.as_str().char_indices() {
      if predicate(c) {
        return Some(o);
      }
    }
    None
  }

  fn slice_index(&self, count: usize) -> Result<usize, Needed> {
    let mut cnt = 0;
    for (index, _) in self.as_str().char_indices() {
      if cnt == count {
        return Ok(index);
      }
      cnt += 1;
    }
    if cnt == count {
      return Ok(self.as_str().len());
    }
    Err(Needed::Unknown)
  }
}

// Copied from impl for `nom::InputIter` for `&'a str` and adapted to handle last input
impl<'a> nom::InputLength for CustomParserInput {
  #[inline]
  fn input_len(&self) -> usize {
    self.as_str().len()
  }
}

impl<'a> nom::InputTake for CustomParserInput {
  #[inline]
  fn take(&self, count: usize) -> Self {
    self.clone_with_read_slice(&self.as_str()[..count])
  }

  // return byte index
  #[inline]
  fn take_split(&self, count: usize) -> (Self, Self) {
    let (prefix, suffix) = self.as_str().split_at(count);
    (self.clone_with_read_slice(suffix), self.clone_with_read_slice(prefix))
  }
}

impl<'a> nom::UnspecializedInput for CustomParserInput {}

// impl<'a> nom::InputTakeAtPosition for CustomParserInput {
//   type Item = char;
//
//   fn split_at_position<P, E: nom::error::ParseError<Self>>(
//     &self,
//     predicate: P,
//   ) -> nom::IResult<Self, Self, E>
//   where
//     P: Fn(Self::Item) -> bool,
//   {
//     match self.find(predicate) {
//       // find() returns a byte index that is already in the slice at a char boundary
//       Some(i) => unsafe {
//         let a = self.as_str().get_unchecked(i..);
//         let b = self.as_str().get_unchecked(..i);
//         Ok((self.clone_with_read_slice(a), self.clone_with_read_slice(b)))
//       },
//       None => Err(nom::Err::Incomplete(Needed::new(1))),
//     }
//   }
//
//   fn split_at_position1<P, E: nom::error::ParseError<Self>>(
//     &self,
//     predicate: P,
//     e: nom::error::ErrorKind,
//   ) -> nom::IResult<Self, Self, E>
//   where
//     P: Fn(Self::Item) -> bool,
//   {
//     match self.find(predicate) {
//       Some(0) => Err(nom::Err::Error(E::from_error_kind(self, e))),
//       // find() returns a byte index that is already in the slice at a char boundary
//       Some(i) => unsafe {
//         let a = self.as_str().get_unchecked(i..);
//         let b = self.as_str().get_unchecked(..i);
//         Ok((self.clone_with_read_slice(a), self.clone_with_read_slice(b)))
//       },
//       None => Err(nom::Err::Incomplete(Needed::new(1))),
//     }
//   }
//
//   fn split_at_position_complete<P, E: nom::error::ParseError<Self>>(
//     &self,
//     predicate: P,
//   ) -> nom::IResult<Self, Self, E>
//   where
//     P: Fn(Self::Item) -> bool,
//   {
//     let inp = self.as_str();
//
//     match self.find(predicate) {
//       // find() returns a byte index that is already in the slice at a char boundary
//       Some(i) => unsafe {
//         let a = inp.get_unchecked(i..);
//         let b = inp.get_unchecked(..i);
//         Ok((self.clone_with_read_slice(a), self.clone_with_read_slice(b)))
//       },
//       // the end of slice is a char boundary
//       None => unsafe {
//         let c = inp.get_unchecked(inp.len()..);
//         let d = inp.get_unchecked(..inp.len());
//         Ok((self.clone_with_read_slice(c), self.clone_with_read_slice(d)))
//       },
//     }
//   }
//
//   fn split_at_position1_complete<P, E: nom::error::ParseError<Self>>(
//     &self,
//     predicate: P,
//     e: nom::error::ErrorKind,
//   ) -> nom::IResult<Self, Self, E>
//   where
//     P: Fn(Self::Item) -> bool,
//   {
//     let inp = self.as_str();
//
//     match self.find(predicate) {
//       Some(0) => Err(nom::Err::Error(E::from_error_kind(self, e))),
//       // find() returns a byte index that is already in the slice at a char boundary
//       Some(i) => unsafe {
//         let a = inp.get_unchecked(i..);
//         let b = inp.get_unchecked(..i);
//         Ok((self.clone_with_read_slice(a), self.clone_with_read_slice(b)))
//       },
//       None => {
//         if self.is_empty() {
//           Err(nom::Err::Error(E::from_error_kind(self, e)))
//         } else {
//           // the end of slice is a char boundary
//           unsafe {
//             let c = inp.get_unchecked(inp.len()..);
//             let d = inp.get_unchecked(..inp.len());
//             Ok((self.clone_with_read_slice(c), self.clone_with_read_slice(d)))
//           }
//         }
//       }
//     }
//   }
// }

impl<'a> nom::Compare<CustomParserInput> for CustomParserInput {
  #[inline(always)]
  fn compare(&self, t: CustomParserInput) -> CompareResult {
    self.as_str().compare(t.as_str())
  }

  #[inline(always)]
  fn compare_no_case(&self, t: CustomParserInput) -> CompareResult {
    self.as_str().compare_no_case(t.as_str())
  }
}
