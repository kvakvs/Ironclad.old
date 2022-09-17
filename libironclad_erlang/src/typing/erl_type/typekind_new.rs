//! Construction for `TypeKind`s

use crate::literal::Literal;
use crate::typing::erl_integer::ErlInteger;
use crate::typing::erl_type::binary_type::{BinaryTypeHeadElement, BinaryTypeTailElement};
use crate::typing::erl_type::map_type::MapMemberType;
use crate::typing::erl_type::typekind::TypeKind;
use crate::typing::erl_type::{ErlType, TypeImpl};
use crate::typing::fn_clause_type::FnClauseType;
use crate::typing::fn_type::FnType;
use crate::typing::record_field_type::RecordFieldType;
use crate::typing::type_union::TypeUnion;
use std::sync::Arc;

impl TypeKind {
  /// Creates new function type with clauses
  pub(crate) fn new_fn_type(clauses: Vec<FnClauseType>) -> TypeKind {
    assert!(!clauses.is_empty(), "Attempt to build a fn type with zero clauses");

    let arity = clauses[0].arity();
    assert!(
      clauses.iter().all(|c| c.arity() == arity),
      "Attempt to build a fn type with clauses of different arity (first clause had arity {})",
      arity
    );

    let fn_type = FnType::new(arity, clauses);
    TypeKind::Fn(fn_type.into())
  }

  /// Creates a new function type with 1 clause, a count of `any()` args and a given return type
  pub fn new_fn_type_of_any_args(arity: usize, ret_ty: ErlType) -> TypeKind {
    // Build an [arity] array of `any()` types
    let any_args = vec![TypeImpl::any(); arity];
    // let any_args = iter::repeat(())
    //   .take(arity)
    //   .map(|_| TypeImpl::any())
    //   .collect();

    let clause = FnClauseType::new(any_args, ret_ty);
    let fn_type = FnType::new(arity, vec![clause]);

    TypeKind::Fn(fn_type.into())
  }

  /// Wrapper to access type union construction
  pub fn new_union(types: &[ErlType]) -> TypeKind {
    match types.len() {
      // Union of 0 types is empty type
      0 => TypeKind::None,
      // Union of 1 type is type itself
      1 => types[0].kind.clone(),
      // Union of many types
      _ => {
        let mut u = TypeUnion::new(types);
        u.normalize();

        // After normalization see if the type did shrink to none() or a singular type
        match u.types.len() {
          0 => panic!("Can't create type union of 0 types after normalization"),
          1 => u.types[0].kind.clone(),
          _ => TypeKind::Union(u),
        }
      }
    }
  }

  /// Create a new union but do not normalize
  #[allow(dead_code)]
  pub(crate) fn new_union_skip_normalize(types: &[ErlType]) -> TypeKind {
    match types.len() {
      0 => TypeKind::None,
      1 => types[0].kind.clone(),
      _ => TypeKind::Union(TypeUnion::new(types)),
    }
  }

  /// Construct a new tuple-type
  #[inline]
  pub fn new_tuple(elements: &[ErlType]) -> TypeKind {
    TypeKind::Tuple { elements: elements.into() }
  }

  /// Construct a new map-type
  #[inline]
  pub(crate) fn new_map(members: Vec<MapMemberType>) -> TypeKind {
    TypeKind::Map { members }
  }

  /// Clones literal's refcounted pointer and returns a singleton type
  #[inline]
  pub(crate) fn new_singleton(lit: Arc<Literal>) -> TypeKind {
    TypeKind::Singleton { val: lit }
  }

  /// Creates a new singleton atom of name `s`
  #[inline]
  pub fn new_atom(s: &str) -> TypeKind {
    TypeKind::Singleton { val: Literal::Atom(s.to_string()).into() }
  }

  /// Creates a type for a proper list with a NIL tail.
  /// If `is_ellipsis` is true, this adds the non-empty list requirement.
  #[inline]
  pub fn list_of(t: ErlType, is_non_empty: bool) -> TypeKind {
    TypeKind::List { elements: t, tail: None, is_non_empty }
  }

  /// Creates a type for a proper list with a NIL tail.
  #[inline]
  pub(crate) fn list_of_types(types: Vec<ErlType>) -> TypeKind {
    TypeKind::StronglyTypedList { elements: types, tail: None }
  }

  /// Creates a type for binary data
  #[inline]
  pub(crate) fn new_binary(
    head: Option<BinaryTypeHeadElement>,
    tail: Option<BinaryTypeTailElement>,
  ) -> TypeKind {
    TypeKind::Binary { head, tail }
  }

  /// Consumes argument.
  /// Construct a new tuple-type
  #[inline]
  pub(crate) fn new_tuple_move(elements: Vec<ErlType>) -> TypeKind {
    TypeKind::Tuple { elements }
  }

  /// Construct a new record reference by tag name
  #[inline]
  pub(crate) fn new_record_ref(tag: String, pins: Vec<RecordFieldType>) -> TypeKind {
    TypeKind::RecordRef { tag, pins }
  }

  /// Construct a new integer range
  #[inline]
  pub(crate) fn new_range(a: ErlInteger, b: ErlInteger) -> TypeKind {
    TypeKind::IntegerRange { from: a, to: b }
  }
}
