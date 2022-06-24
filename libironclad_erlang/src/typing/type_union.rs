//! Union type (a flat list of multiple types) support

use crate::typing::erl_type::{ErlType, ErlTypeImpl};
use std::ops::Deref;
use std::sync::Arc;

/// Contains multiple types
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeUnion {
  /// Member types of a type union
  pub types: Vec<ErlType>,
}

impl TypeUnion {
  /// True if union contains no types and is equal to none() type
  pub(crate) fn is_empty(&self) -> bool {
    self.types.is_empty()
  }

  /// Create a type union from a vec of types. Nested unions are unwrapped and joined
  pub(crate) fn new(types: &[ErlType]) -> Self {
    // throw away none() types
    let types = types
      .iter()
      .filter(|t| !t.is_none())
      // Fold the input type list and flatten nested unions
      .fold(Vec::<ErlType>::new(), |mut accum, item| {
        if item.is_union() {
          accum.extend(item.as_union().types.iter().cloned())
        } else {
          accum.push(item.clone())
        }
        accum
      });
    Self { types }
  }

  /// Function to filter type union members.
  /// An union member is kept if for every other union member it's not a subtype of the other member.
  /// Or it is an equivalent to the other member and this is the first equivalent arm
  fn is_subtype_of_another_union_member(&self, every_index: usize, every_type: &ErlType) -> bool {
    for every_other_index in every_index + 1..self.types.len() {
      let every_other_type = &self.types[every_other_index];
      if every_type.is_subtype_of(every_other_type) {
        return true;
      }
    }
    false
  }

  /// Filters through the types in the union and throws away those which are subtypes of other type
  /// in the same union
  pub(crate) fn normalize(&mut self) {
    // Merge int()|float() into number()
    if self.contains_strict(&ErlTypeImpl::integer()) && self.contains_strict(&ErlTypeImpl::float())
    {
      self.types = self
        .types
        .iter()
        .filter(|t| !t.is_integer() && !t.is_float()) // throw away int and floats
        .cloned()
        .collect();
      self.types.push(ErlTypeImpl::number()); // replace int and float with number()
    }

    // Remove the subtypes for other types in the union
    self.types = self
      .types
      .iter()
      .filter(|t| !t.is_none()) // throw away none() types
      .enumerate()
      .filter(|(index1, type1)| !self.is_subtype_of_another_union_member(*index1, type1))
      .map(|(_index, ty)| ty.clone()) // unwrap type value from enumerate pairs
      .collect();
  }

  /// Whether type t is found in any of the union contents (is_subtype_of equality)
  pub fn contains(&self, t: &ErlTypeImpl) -> bool {
    self.types.iter().any(|member| t.is_subtype_of(member))
  }

  /// Whether type t is found in any of the union contents (strict equality)
  pub(crate) fn contains_strict(&self, t: &ErlTypeImpl) -> bool {
    self.types.iter().any(|member| t == member.deref())
  }
}
