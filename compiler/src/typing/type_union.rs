//! Union type (a flat list of multiple types) support

use std::sync::Arc;
use crate::typing::erl_type::ErlType;

/// Contains multiple types
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeUnion {
  types: Vec<Arc<ErlType>>,
  // lists: Vec<Arc<ErlType>>,
  // tuples: Vec<Arc<ErlType>>,
  // records: Vec<Arc<ErlType>>,
  // maps: Vec<Arc<ErlType>>,
  // binaries: Vec<Arc<ErlType>>,
}

impl TypeUnion {
  /// True if union contains no types and is equal to none() type
  pub fn is_empty(&self) -> bool { self.types.is_empty() }

  /// Access the readonly types list
  pub fn types(&self) -> &Vec<Arc<ErlType>> { &self.types }

  /// Create a type union from a vec of types. Nested unions are unwrapped and joined
  pub fn new(types: &[Arc<ErlType>]) -> Self {
    // throw away none() types
    let types = types.iter()
        .filter(|t| !t.is_none())
        // Fold the input type list and flatten nested unions
        .fold(Vec::<Arc<ErlType>>::new(),
              |mut accum, item| {
                if item.is_union() {
                  accum.extend(item.as_union().types.iter().cloned())
                } else {
                  accum.push(item.clone())
                }
                accum
              });
    Self { types }
  }

  /// Filters through the types in the union and throws away those which are subtypes of other type
  /// in the same union
  pub fn normalize(&mut self) {
    // let self_clone = self.types.clone();
    self.types = self.types.iter()
        .filter(|t| !t.is_none()) // throw away none() types
        .enumerate()
        .filter(|(index1, type1)| {
          // an arm is kept if for every arm (except itself) it's not a subtype of the other arm
          // or it's equivalent to the other arm and this is the first equivalent arm
          return self.types.iter().enumerate()
              .all(|(index2, type2)|
                       *index1 == index2
                           || !type1.is_subtype_of(type2)
                           || (type2.is_subtype_of(type1) && *index1 < index2),
              );
        })
        .map(|(_index, ty)| ty.clone())
        .collect();
    // match new_types.len() {
    //   0 => ErlType::None.into(),
    //   1 => new_types[0].clone(),
    //   _ => ErlType::Union(TypeUnion::new(new_types)).into() // rebuild from remaining types
    // }
  }

  /// Whether type t is found in any of the union contents
  pub fn contains(&self, t: &ErlType) -> bool {
    self.types.iter()
        .any(|union_t| t.is_subtype_of(union_t))
  }
}
