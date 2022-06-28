//! Collection of MFArity which is lockable

use crate::pretty::Pretty;
use std::fmt::{Display, Formatter};
use std::sync::RwLock;

/// Vec of generic value type, protected by a `RwLock`
#[derive(Debug)]
pub struct RwVec<ValType> {
  /// The lockable array
  pub data: RwLock<Vec<ValType>>,
}

impl<ValType: Display> Display for RwVec<ValType> {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    if let Ok(r_vec) = self.data.read() {
      Pretty::display_square_list(r_vec.iter(), f)
    } else {
      panic!("Can't lock RwVec for printing")
    }
  }
}

impl<ValType> Default for RwVec<ValType> {
  fn default() -> Self {
    Self { data: RwLock::new(Vec::default()) }
  }
}

impl<ValType> RwVec<ValType> {
  /// Create a `RwVec` with preallocated capacity
  pub fn with_capacity(cap: usize) -> Self {
    Self { data: RwLock::new(Vec::with_capacity(cap)) }
  }

  /// Contained data length
  pub fn len(&self) -> usize {
    if let Ok(r_data) = self.data.read() {
      r_data.len()
    } else {
      panic!("Can't lock RwVec for length check")
    }
  }

  /// Check whether the vector length is empty
  pub fn is_empty(&self) -> bool {
    if let Ok(r_data) = self.data.read() {
      r_data.is_empty()
    } else {
      panic!("Can't lock RwVec for empty check")
    }
  }

  // /// Check whether an item exists
  // pub fn contains(&self, key: &ValType) -> bool {
  //   if let Ok(r_data) = self.data.read() {
  //     r_data.contains(key)
  //   } else {
  //     panic!("Can't lock RwVec for reading")
  //   }
  // }

  /// Append an item to the end of the vector
  pub fn push(&self, item: ValType) {
    if let Ok(mut w_data) = self.data.write() {
      w_data.push(item);
    } else {
      panic!("Can't lock RwVec to push")
    }
  }
}
