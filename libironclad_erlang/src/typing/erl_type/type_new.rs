//! Constructors for complex ErlTypes

use crate::literal::Literal;
use crate::typing::erl_integer::ErlInteger;
use crate::typing::erl_type::binary_type::{BinaryTypeHeadElement, BinaryTypeTailElement};
use crate::typing::erl_type::map_type::MapMemberType;
use crate::typing::erl_type::ErlTypeImpl::UserDefinedType;
use crate::typing::erl_type::{ErlType, ErlTypeImpl};
use crate::typing::fn_clause_type::FnClauseType;
use crate::typing::fn_type::FnType;
use crate::typing::record_field_type::RecordFieldType;
use crate::typing::type_union::TypeUnion;
use crate::typing::typevar::Typevar;
use libironclad_util::mfarity::MFArity;
use std::iter;
use std::sync::Arc;

//
// Constructors and Generators
//
impl ErlTypeImpl {
  /// Clones literal's refcounted pointer and returns a singleton type
  pub(crate) fn new_singleton(lit: &Arc<Literal>) -> ErlType {
    ErlTypeImpl::Singleton { val: lit.clone() }.into()
  }

  /// Creates a new singleton atom of name `s`
  pub fn new_atom(s: &str) -> ErlType {
    ErlTypeImpl::Singleton { val: Literal::Atom(s.to_string()).into() }.into()
  }

  /// Creates a type for a proper list with a NIL tail.
  /// If `is_ellipsis` is true, this adds the non-empty list requirement.
  pub fn list_of(t: ErlType, is_non_empty: bool) -> ErlType {
    let result = ErlTypeImpl::List { elements: t, tail: None, is_non_empty };
    result.into()
  }

  /// Creates a type for a proper list with a NIL tail.
  pub(crate) fn list_of_types(types: Vec<ErlType>) -> ErlType {
    let result = ErlTypeImpl::StronglyTypedList { elements: types, tail: None };
    result.into()
  }

  /// Creates a type for binary data
  pub(crate) fn new_binary(
    head: Option<BinaryTypeHeadElement>,
    tail: Option<BinaryTypeTailElement>,
  ) -> ErlType {
    ErlTypeImpl::Binary { head, tail }.into()
  }

  /// Creates new function type with clauses
  pub(crate) fn new_fn_type(clauses: Vec<FnClauseType>) -> ErlTypeImpl {
    assert!(!clauses.is_empty(), "Attempt to build a fn type with zero clauses");

    let arity = clauses[0].arity();
    assert!(
      clauses.iter().all(|c| c.arity() == arity),
      "Attempt to build a fn type with clauses of different arity (first clause had arity {})",
      arity
    );

    let fn_type = FnType::new(arity, clauses);
    ErlTypeImpl::Fn(fn_type.into())
  }

  /// Creates a new function type with 1 clause, a count of `any()` args and a given return type
  pub fn new_fn_type_of_any_args(arity: usize, ret_ty: &ErlType) -> ErlTypeImpl {
    let any_args = iter::repeat(())
      .take(arity)
      .map(|_| Typevar::new(None, None))
      .collect();
    let clause = FnClauseType::new(any_args, Typevar::from_erltype(ret_ty));
    let fn_type = FnType::new(arity, vec![clause]);
    ErlTypeImpl::Fn(fn_type.into())
  }

  /// Creates a new any-function type
  #[inline]
  pub fn new_any_fn_type() -> ErlType {
    ErlTypeImpl::AnyFn.into()
  }

  /// Wrapper to access type union construction
  pub fn new_union(types: &[ErlType]) -> ErlType {
    match types.len() {
      // Union of 0 types is empty type
      0 => ErlTypeImpl::none(),
      // Union of 1 type is type itself
      1 => types[0].clone(),
      // Union of many types
      _ => {
        let mut u = TypeUnion::new(types);
        u.normalize();

        // After normalization see if the type did shrink to none() or a singular type
        match u.types.len() {
          0 => panic!("Can't create type union of 0 types after normalization"),
          1 => u.types[0].clone(),
          _ => ErlTypeImpl::Union(u).into(),
        }
      }
    }
  }

  /// Create a new union but do not normalize
  #[allow(dead_code)]
  pub(crate) fn new_union_skip_normalize(types: &[ErlType]) -> ErlType {
    match types.len() {
      0 => ErlTypeImpl::none(),
      1 => types[0].clone(),
      _ => ErlTypeImpl::Union(TypeUnion::new(types)).into(),
    }
  }

  /// Construct a new tuple-type
  pub fn new_tuple(elements: &[ErlType]) -> ErlType {
    ErlTypeImpl::Tuple { elements: elements.into() }.into()
  }

  /// Construct a new map-type
  pub(crate) fn new_map(members: Vec<MapMemberType>) -> ErlType {
    ErlTypeImpl::Map { members }.into()
  }

  /// Consumes argument.
  /// Construct a new tuple-type
  pub(crate) fn new_tuple_move(elements: Vec<ErlType>) -> ErlType {
    ErlTypeImpl::Tuple { elements }.into()
  }

  /// Construct a new type variable wrapper
  pub(crate) fn new_typevar(tv: Typevar) -> ErlType {
    ErlTypeImpl::Typevar(tv).into()
  }

  /// Construct a new record reference by tag name
  pub(crate) fn new_record_ref(tag: String, pins: Vec<RecordFieldType>) -> ErlType {
    ErlTypeImpl::RecordRef { tag, pins }.into()
  }

  /// Construct a new integer range
  pub(crate) fn new_range(a: ErlInteger, b: ErlInteger) -> ErlType {
    ErlTypeImpl::IntegerRange { from: a, to: b }.into()
  }

  /// Try match type name and arity vs known basic types
  pub(crate) fn from_name(
    maybe_module: Option<String>,
    type_name: String,
    args: &[Typevar],
  ) -> ErlType {
    #[allow(clippy::single_match)]
    match args.len() {
      0 => match type_name.as_ref() {
        "any" => return ErlTypeImpl::any(),
        "none" => return ErlTypeImpl::none(),

        "number" => return ErlTypeImpl::number(),
        "integer" => return ErlTypeImpl::integer(),
        "float" => return ErlTypeImpl::float(),

        "atom" => return ErlTypeImpl::atom(),
        "boolean" => return ErlTypeImpl::boolean(),

        "list" => return ErlTypeImpl::any_list(),
        "nil" => return ErlTypeImpl::nil(),

        "tuple" => return ErlTypeImpl::any_tuple(),

        "pid" => return ErlTypeImpl::pid(),
        "port" => return ErlTypeImpl::port(),
        "reference" => return ErlTypeImpl::reference(),
        _ => {}
      },
      _ => {}
    }
    // We were not able to find a basic type of that name and arity
    UserDefinedType {
      name: MFArity::new_opt(maybe_module, &type_name, args.len()),
      args: args.to_vec(),
    }
    .into()
  }
}
